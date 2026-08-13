# TCP Accept-Queue Hardening

**Status:** Completed-child queue mechanics implemented; half-open admission and overflow cleanup incomplete, with fixed backlog policy provisional
**Date:** 2026-08-12 review

## Problem

The current `TCP-INPUT-LISTEN` handler mutates the *listener's own TCB*
when a SYN arrives, transitioning it through SYN-RCVD → ESTABLISHED.
This means:

1. **Dropped connections** — between the moment the listener TCB leaves
   LISTEN and the moment `SOCK-ACCEPT` re-opens a new listener via
   `TCP-LISTEN`, any SYN arriving on that port is silently dropped.
2. **Concurrency fragility** — the design assumes single-connection-at-a-time
   processing.  Two rapid SYNs race for the same TCB.
3. **Stack leak** — `TCP-INPUT-LISTEN` declares `( tcb -- )` but actually
   leaves the original tcb on the data stack (`DUP >R` should be `>R`).

## Solution: per-listener accept queue

### Design

- Expand `/TCB` from 5728 → 5816 bytes (+88 bytes) to embed a small
  accept queue directly in each TCB:
  - `+5724  AQ-HEAD    1 cell`  — circular-queue read index
  - `+5732  AQ-TAIL    1 cell`  — circular-queue write index
  - `+5740  AQ-COUNT   1 cell`  — entries currently queued
  - `+5748  AQ-SLOTS   8 cells` — 8 pointers to completed TCBs (64 bytes)
  - `+5812  (pad to 5816)`

- Non-listener TCBs pay 88 bytes of unused space.  At 256 max connections
  this is ~22 KB — well within XMEM budget.
- Completed-queue storage is inline. Listener close drains queued children;
  half-open child cleanup remains incomplete as described below.

### Accept-queue capacity: 8 entries

Eight slots is the current inline implementation capacity. If the queue is
full when a new SYN reaches the listener, the SYN is ignored before a child
TCB is allocated and the peer must recover through its retry behavior. However,
the check counts only completed children. More than eight SYN-RCVD children can
exist concurrently; when their ACKs arrive, failed `AQ-PUSH` results are
discarded and can leave orphan established TCBs. Secure accept must reserve
half-open admission or reclaim on overflow and test that case explicitly.

This document does not claim that eight covers every deployment. A caller- or
configuration-derived backlog remains tracked production capacity work, but it
does not replace the immediate requirement for exact safe overload behavior.

### Changed words

| Word | Change |
|------|--------|
| `/TCB` | 5728 → 5816 |
| `TCB.AQ-HEAD` | New accessor (+5724) |
| `TCB.AQ-TAIL` | New accessor (+5732) |
| `TCB.AQ-COUNT` | New accessor (+5740) |
| `TCB.AQ-SLOTS` | New accessor (+5748) |
| `AQ-FULL?` | New: check if accept queue is full |
| `AQ-PUSH` | New: enqueue a TCB pointer |
| `AQ-POP` | New: dequeue a TCB pointer |
| `TCP-INPUT-LISTEN` | Allocate fresh TCB for connection; listener stays in LISTEN.  Fix stack leak. |
| `TCP-INPUT-ESTABLISHED-ETC` | At SYN-RCVD → ESTABLISHED transition, enqueue new TCB into listener's accept queue (found via `TCB-FIND-LPORT`). |
| `TCP-LISTEN` | Initialise accept-queue fields (head=0, tail=0, count=0). |
| `TCP-CLOSE` (LISTEN case) | Drain accept queue: close any pending TCBs before resetting listener. |
| `SOCK-ACCEPT` | Dequeue from the accept queue instead of transplanting the listener TCB.  No re-open is needed.  Refuse a TLS-marked listener before removing a queued child. |
| `LISTEN` (socket API) | Continue to open ordinary TCP listeners.  A TLS-marked descriptor now returns `-1` without allocating a listener TCB or changing its descriptor state/handle; secure accept remains unavailable until the authenticated server path exists. |
| `NET-TABLES-INIT` | Budget the complete logical per-connection allocation: 5,816-byte `/TCB` + 968-byte `/TLS-CTX` + 230,688-byte `/TLS-RX-WORKSPACE` + two 32-byte socket descriptors = 237,536 bytes. The workspace adds a full 131,146-byte ClientHello lane, an 8,192-byte bitmap covering all 65,536 extension types, and a 512-byte immutable server-flight ledger plus 200 bytes of exact metadata. XMEM capacity uses independently normalized table allocations: one connection reserves 237,552 bytes, two reserve 475,072, and odd counts carry 16 bytes of aggregate padding. |

### Unchanged words

- `TCB-ALLOC`, `TCB-INIT`, `TCB-FIND`, `TCB-FIND-LPORT` — no changes needed.
- `TCP-CONNECT`, `TCP-SEND`, `TCP-RECV` — unaffected.
- `TCP-LISTEN` remains the ordinary TCP passive-open primitive; the socket API
  calls it only for a TCP-marked descriptor.
- Ring buffer (§18) — not used; accept queue is self-contained inline.

### Related TCP qualification boundary

Keeping a listener in LISTEN and retaining one accepted child queue does not
qualify the data-delivery path. The current one-outstanding-segment sender has
open ACK-range, partial-ACK retained-suffix, retransmission-sequence,
RTO-service, and advertised-window defects; send admission can block in ARP resolution, and passive control/FIN
replay and active-open state validation are incomplete. Those require a narrow
focused repair before outbound TLS server replay relies on the TCB's retained
ciphertext.

### Qualification inventory

- `/TCB` size assertion (5728 → 5816).
- `TCB-N` diff assertion (5728 → 5816).
- `test_socket_listen_accept` exercises the full SYN → SYN-ACK → ACK → ACCEPT
  path.
- `test_aq_push_pop` verifies AQ-PUSH/AQ-POP semantics.
- `test_listener_stays_listening` verifies that after SYN processing, listener
  TCB remains in TCPS-LISTEN state.
- `test_aq_full_rejects` verifies that a ninth direct `AQ-PUSH` is rejected.
- A wire-level test with more than eight half-open children that subsequently
  ACK remains part of the secure-accept qualification plan; it must prove
  overflow cleanup and absence of orphan TCBs.
