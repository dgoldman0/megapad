# TCP Accept-Queue and Authority Hardening

**Status:** Incarnation-safe TCB/TLS/socket ownership, bounded active and
passive control transport, and retained FIN completion are implemented; the
accepted-child TLS adapter and secure TLS socket accept are not implemented
**Date:** 2026-08-14 qualification

## Scope

The listener keeps its own TCB in `LISTEN` while each admitted SYN receives a
fresh child TCB. The child is not public authority merely because code has its
address: allocation, passive lineage, queueing, attachment, close, and abort
are now checked against an exact TCB generation and owner.

This milestone supplies the transport authority needed by secure accept. It
does not yet attach a queued child to a server TLS context or publish an
authenticated TLS socket.

## TCB and table geometry

The current `/TCB` is 5,952 bytes. The accept queue remains inline with eight
slots; the authority fields added after retained transport state are:

| Offset | Field | Meaning |
|--------|-------|---------|
| +5832 | `GENERATION` | Nonzero slot incarnation; stale values do not resolve |
| +5840 | `PARENT-H1` | Passive listener slot+1 while the child is unclaimed |
| +5848 | `PARENT-GEN` | Exact listener incarnation |
| +5856 | `OWNER` | Attached socket or TLS-context address |
| +5864 | `AUTH-STATE` | none, half-open, queued, or attached |
| +5872 | `AQ-RESERVED` | Half-open plus completed backlog reservations |
| +5880 | `AQ-GENS` | Eight generations paired with `AQ-SLOTS` |
| +5944 | `CONTROL-STALL` | First due control replay that local admission blocked |

`/TLS-CTX` is 1,000 bytes. It carries the attached TCB generation at +968,
its own nonzero incarnation at +976, the reciprocal socket owner at +984, and
the slot/close lifecycle at +992. `TLS-CLOSE-FREE` marks a released slot while
preserving its last generation, so one successful claim creates one live
incarnation. `/SOCK` is 40 bytes and carries either the
plain TCB generation or TLS-context generation at +32. With the 230,688-byte
TLS receive/server workspace, the logical table cost is 237,720 bytes per
connection. Independently normalized XMEM allocations consume 237,728,
475,440, and 713,168 bytes for one, two, and three connections.

## Incarnation and ownership rules

- Claiming a free TCB increments its generation before the slot becomes
  usable. A generation that reaches the retirement boundary is never wrapped
  into a value that could revive stale authority.
- `TCB-HANDLE-RESOLVE` accepts only an in-range `(slot+1, generation)` for a
  live, non-reserved incarnation. A raw address or matching four-tuple is not
  ownership.
- `TCP-ATTACH` admits only a live listener, active-open, established, or
  close-wait TCB. The owner must be nonzero, the TCB must be unattached, and
  one owner may not attach to two TCBs.
- `TCB-ATTACHED-TO?` is the reciprocal check: exact TCB address, generation,
  attached state, and owner must all match. Socket and TLS dereferences use
  that check.
- Raw `TCP-CLOSE[-TRY]` and `TCP-ABORT[-TRY]` reject attached TCBs.
  `TCP-OWNER-CLOSE` and `TCP-OWNER-ABORT` validate the complete token and
  perform the lifecycle transition under the network lock. A stale token or
  failed close cannot detach the real owner.

TLS contexts store both the TCB address and generation. A TLS socket adds a
second reciprocal edge: the descriptor stores `(context, context-generation)`
while the context stores that exact descriptor as `SOCKET-OWNER`. Publication
sets both sides under the TLS-then-network lock order, and socket I/O resolves
both the socket/context edge and the context/TCB edge before use. Raw-context
entry points reject a socket-owned context. Socket I/O, close, and abort cannot
use a stale captured context or TCB binding across teardown and reuse. The bare
raw-context and descriptor-pointer interfaces themselves remain
lifetime-scoped; neither pointer alone is an opaque generational handle.

## Passive admission and accept transfer

`AQ-RESERVED` accounts for every half-open and queued child. The listener
reserves capacity before allocating a child, so at most eight children can
occupy the combined passive backlog. Allocation, initial SYN+ACK emission,
terminal failure, retry exhaustion, listener teardown, and queue removal each
release or transfer that reservation exactly once.

The bounded passive profile admits only an exact bare SYN with no payload.
The child records the listener's `(slot+1, generation)`, enters
`TCP-AUTH-HALF-OPEN`, and retains its SYN+ACK control intent. The final segment
must use the expected sequence and acknowledge the emitted SYN before the
child can become established and queued.

Queue slots contain `(child slot+1, child generation)`, not reusable raw
pointers. `TCP-ACCEPT-CLAIM` validates:

- the listener's exact attached generation and owner;
- the queued child's exact live generation;
- the child's exact parent-listener token and queued authority state;
- an established or close-wait child with no existing owner.

Only then does it dequeue the entry and atomically transfer the child to the
new descriptor owner. `SOCK-ACCEPT` reserves a descriptor before consuming the
queue, and the accepted socket publishes both the child address and generation.
Listener close/abort scans exact parent tokens and reclaims both half-open and
queued children.

Eight is the current inline backlog policy, not a universal capacity claim.
A caller- or configuration-derived backlog remains later capacity work; it is
not required to make the present fixed backlog safe.

## Active open, retry, and graceful-close behavior

Active open retains its SYN control intent at the original ISS and replays it
with bounded exponential RTO. `SYN-SENT` admits only a payload-free segment
whose flags are exactly SYN+ACK and whose ACK is exactly `ISS+1`; a bare SYN is
not simultaneous-open support and is ignored. Establishment durably schedules
the final ACK. If that ACK is lost, an exact duplicate SYN+ACK carrying the
original peer sequence and `ISS+1` is acknowledged again without reopening or
otherwise mutating the established connection.

The cooperative maintenance scan replays a half-open SYN+ACK from the original
ISS with bounded exponential RTO. Wire retry counters advance only after an
actual NIC admission. A separate bounded control-stall timer covers unresolved
neighbor or persistent NIC backpressure, so active SYN, passive SYN+ACK, and
FIN control intents cannot remain live forever without consuming a wire retry.
Half-open expiry reclaims the child and releases its reservation.

Graceful owner close does not send FIN while retained application or TLS bytes
remain unacknowledged. `TLS-CLOSE-TRY` first admits an exact protected
`close_notify`; TCP retains that record until ACK, and close returns
`TLS-E-WOULD-BLOCK` while it remains in flight. A later retry can emit FIN only
after the record is acknowledged. FIN-WAIT-1, CLOSING, and LAST-ACK retain the
FIN sequence intent and replay it with bounded exponential RTO. FIN-WAIT-2 has
a separate 60-second terminal timeout. TIME-WAIT protects the old four-tuple
for 2MSL; an exact duplicate FIN is re-ACKed and restarts that quarantine. The
TLS context is wiped only after its exact owner-qualified close is admitted.

`TLS-CLOSE-TRY` and `TLS-CLOSE` are both checked `(ctx -- ior)` operations;
retryable failure retains the context. `TLS-CLOSE-FINAL` spends its bounded
graceful-progress budget and then falls back to exact abort: zero means the
context has actually been disposed, while nonzero retains its context token for
retry. Abort fallback may already have reclaimed transport before a contended
credential unpin returns busy; the claimed context and exact pin metadata remain
available for retry. `TLS-ABORT` immediately reclaims an exact raw-context binding without
`close_notify`; `SOCK-ABORT` provides the corresponding exact descriptor API
and returns both the transport disposition and an `ior`. `CLOSE-TRY` and
`CLOSE` are likewise checked `(sd -- ior)` operations: zero means the
descriptor was released, while nonzero preserves retry authority.

The one-segment retained-data profile is unchanged: exact TLS records are
all-or-none, cumulative ACKs are wrap-safe, partial ACKs trim the retained
prefix, and data failure remains owner-visible until explicit cleanup.

## Qualification

Final sequential source-mode evidence for this milestone is 277/277
`TestKDOSNetStack`, 38/38 `TestKDOSTLSAppData`, 21/21 `TestKDOSSocket`,
161/161 `TestKDOSTLS`, 28/28 `TestToolsModule`, and 65/65 adjacent
hardening/source-selection tests. The four-core server-flight and credential
cancellation capstones passed separately in 701.122 and 520.361 seconds. Their
snapshot fixture proves complete KDOS and networking source loads before
saving state; networking has a measured 450,000,000-step construction ceiling,
while each capstone retains its independent 400,000,000-step execution ceiling.

## Remaining secure-server boundary

The authority substrate is not a secure TLS accept API. TLS-marked `LISTEN`
and `SOCK-ACCEPT` still fail closed before consuming a child. Remaining work
includes:

- claim a queued child directly into a prepared server TLS context;
- adapt the qualified server-flight emitter and client-flight ingress to the
  exact attached TCB without exposing plaintext;
- publish an accepted socket only after client Finished authentication and
  explicit TLS establishment;
- qualify the complete socket lifecycle and close against an independent TLS
  1.3 implementation.
