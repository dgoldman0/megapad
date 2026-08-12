# TCP Accept-Queue Hardening

**Status:** Complete
**Date:** 2026-03-06

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
- No dynamic allocation, no leak on listener close.

### Accept-queue capacity: 8 entries

8 slots covers all practical scenarios.  If the queue is full when a new
SYN arrives, the SYN is silently dropped; the client retries per TCP
spec (exponential backoff).  This is standard behaviour — Linux defaults
to a backlog of 5 for `listen()`.

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
| `NET-TABLES-INIT` | Budget the complete logical per-connection allocation: 5,816-byte `/TCB` + 968-byte `/TLS-CTX` + 230,648-byte `/TLS-RX-WORKSPACE` + two 32-byte socket descriptors = 237,496 bytes. The workspace adds a full 131,146-byte ClientHello lane, a 65,536-type duplicate-extension bitmap, and a 512-byte immutable server-flight ledger plus 160 bytes of exact metadata. XMEM capacity uses independently normalized table allocations: one connection reserves 237,520 bytes, two reserve 474,992, and odd counts carry 24 bytes of aggregate padding. |

### Unchanged words

- `TCB-ALLOC`, `TCB-INIT`, `TCB-FIND`, `TCB-FIND-LPORT` — no changes needed.
- `TCP-CONNECT`, `TCP-SEND`, `TCP-RECV` — unaffected.
- `TCP-LISTEN` remains the ordinary TCP passive-open primitive; the socket API
  calls it only for a TCP-marked descriptor.
- Ring buffer (§18) — not used; accept queue is self-contained inline.

### Test plan

- Update `/TCB` size assertion (5728 → 5816).
- Update `TCB-N` diff assertion (5728 → 5816).
- Existing `test_socket_listen_accept` — should pass unchanged (exercises
  full SYN → SYN-ACK → ACK → ACCEPT path).
- New: `test_accept_queue_basics` — verify AQ-PUSH/AQ-POP semantics.
- New: `test_listener_stays_listening` — after SYN processing, listener
  TCB remains in TCPS-LISTEN state.
- New: `test_accept_queue_full_drops_syn` — 8 rapid SYNs fill the queue,
  9th is silently dropped.
