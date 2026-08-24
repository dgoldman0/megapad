# TCP Accept-Queue and Authority Hardening

**Status:** Incarnation-safe TCB/TLS/socket ownership, bounded active and
passive control transport, retained FIN completion, and atomic queued-child
attachment to a prepared TLS server context are implemented. A sealed
server-flight step now admits the exact ServerHello over that child, preserves
retry state, reclaims a dead exact child, and leaves a reused incarnation
untouched while clearing stale local authority. Initial ClientHello ingress now
reassembles arbitrary TCP and TLSPlaintext-record fragmentation through that
same exact child without overreading the following record. The existing
attached emitter is qualified through all ACK-paced protected records and
server Finished. Attached protected ingress now authenticates client Finished
through the exact child and preserves a following TCP record. Attached
terminal disposition now emits an exact protected fatal/close response or no
response for a non-close peer alert, with retry-stable ciphertext and
alert-ACK-before-FIN ordering. Exact authenticated TLS socket publication and
atomic credential-pinned listener policy publication are implemented. KDOS
ends at a compact generation-exact substrate: configured listener publication,
fused secure-child claim, bounded handshake/transport phases, authenticated
socket publication, and exact close or abort. Akashic's persistent inbound
owner supplies accept scheduling, deadlines, cancellation, retained results,
cooperative cleanup, and adoption into the shared established TLS NIO port.
Independent TLS 1.3 peers qualify application I/O and teardown through that
owner and HCONN, including same-listener recovery after cancellation, timeout,
and malformed input.
**Date:** 2026-08-15 transport qualification

**M* integration closure:** 2026-08-23 at qualified MegaPad code `ca02a40`

## Scope

The listener keeps its own TCB in `LISTEN` while each admitted SYN receives a
fresh child TCB. The child is not public authority merely because code has its
address: allocation, passive lineage, queueing, attachment, close, and abort
are now checked against an exact TCB generation and owner.

This milestone supplies the transport authority needed by secure accept, and
`TLS-SERVER-CONTEXT-BEGIN` now returns the newly claimed context generation,
and `TLS-SERVER-ACCEPT-ATTACH` requires that carried token before it transfers
one exact queued child into the prepared TLS server context. A stale context
incarnation is rejected before accept-queue mutation. `TLS-SERVER-FLIGHT-STEP`
consumes that sealed authority for outbound records without exposing a caller
callback. `TLS-SERVER-CLIENT-HELLO-STEP` consumes the same attached authority
for bounded initial ingress; each ClientHello-fragment record may use legacy
version `0x0301` or `0x0303`. It retains incomplete record/message prefixes per
context and admits exactly one complete ClientHello.
`TLS-SERVER-CLIENT-FLIGHT-BEGIN-ATTACHED` and
`TLS-SERVER-CLIENT-FLIGHT-STEP` then retain that seal while authenticating at
most one protected record per step.
`TLS-SERVER-INGRESS-DISPOSITION-STEP` then consumes only the sticky terminal
classification and exact context generation. Its protected response reuses
the completed emitter's pending lane across TCP/NET backpressure; a peer alert
that is not close_notify intentionally produces no TLS record.
`TLS-SERVER-SOCKET-PUBLISH` consumes only a successful attached client-flight
boundary and the exact context generation. Under TLS-to-credential-to-NET lock
order it proves the still-pinned context, exact child, and descriptor capacity
before releasing the credential reference, publishing handshake state, and
creating reciprocal descriptor/context authority. Bound server contexts cannot
bypass this transaction through generationless `TLS-HANDSHAKE-PUBLISH`; a
retained transport seal keeps that gate closed even when retryable stale-abort
cleanup has already cleared the live TCB fields.

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
incarnation. `/SOCK` is 344 bytes: its common header carries either the plain
TCB generation or TLS-context generation at +32, and its protocol-bounded tail
holds one copied secure-listener policy, including at most one 255-byte ALPN
ProtocolName. With the 230,688-byte TLS receive/server workspace, the logical
table cost is 238,328 bytes per connection. Independently normalized XMEM
allocations consume 238,336, 476,656, and 714,992 bytes for one, two, and three
connections.

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
Authenticated plaintext already retained by TLS is delivered before a later
terminal TCP failure is published. Raw and descriptor status, readiness, and
send probes therefore cannot wipe `APP-LEN`; after the final retained byte is
drained, the next observation publishes the sticky transport error and
reclaims the exact failed TCB.

## Qualification

Focused sequential source-mode evidence for this delta is 8/8 adjacent
accept/emitter/ingress tests plus 4/4 owner-I/O lifecycle tests. It covers exact
ServerHello bytes, retained zero-window retry, dead-versus-backpressure
classification, generic callback exclusion, exact child reclamation, reused
incarnation isolation, exception-fallback authority retention, listener
preservation, and the unchanged socket-independent Finished path. Broader
lower baselines and four-core credential/server-flight cancellation capstones
remain regression inventory rather than prerequisites for each narrow commit.
The subsequent initial-ingress slice passes 5/5 focused tests covering raw
parser exclusion after attachment, real Ethernet/IP/TCP segmentation,
TLSPlaintext fragmentation across `0x0301` and `0x0303` records, exact
following-record retention, fatal framing/handshake alerts, EOF reclamation,
and stale-incarnation isolation.
The attached protected-ingress slice adds segmented independent Finished,
raw/stale authority exclusion, exact following-record retention, explicit TLS
publication, and partial-record EOF reclamation. A final sequential 11-test
affected selector passed under the ordinary checked source-mode limits.
The attached disposition slice adds independent protected fatal/close wire
oracles, byte-identical retry, close-alert ACK before FIN, peer-alert
no-response, and exact reused-child isolation. Its final affected selector,
including ordinary sealed-emitter and TLS-abort paths, passed 19/19
sequentially under the checked source-mode limits.
The authenticated-publication slice adds seal-history-aware generationless
raw-publish refusal,
reciprocal descriptor/context resolution, credential/NET/capacity retry with
byte-identical TLS, TCB, and server metadata, publication of a close-wait child,
descriptor-owned teardown, and stale child-reuse isolation. Its focused matrix
and final adjoining affected selector pass 15/15 sequentially under the
ordinary checked limits.

## 2026-08-23 M* integration closure

The qualified M* code checkpoint is
`ca02a40c04840791c731dbb7c77ecd7e85eb4909` on
`integration/secure-registry-burrow`. It retains the secure-server transport
contracts and the capacity-derived module-registry integration described below,
then exposes `PROVIDED-SPAN` as the public caller-owned exact-ID registration
path required by paired binary-image loaders. That addition uses the same
dynamic Bank-0 registry transaction as parsed `PROVIDED`; it does not change
TCP/TLS authority, wire behavior, table geometry, ownership, or close semantics.

At exact M* code `ca02a40`, the in-sandbox sequential sweep passed 3,614 tests,
skipped 36, and deselected the four host-loopback UDP-backend cases in 1,502.26
seconds. The focused dynamic-registry selector passed 11/11, and the
module/`PROVIDED` guard selector passed 69/69. The earlier `8f0e478` checkpoint
remains the historical source of the `BALANCE` correction and adjoining
`TestKDOSMulticore` result of 87/87 in 854.40 seconds. Its 11 other sandboxed
networking cases also remain valid adjoining evidence. Final host-environment
confirmation at exact `ca02a40` passed all four AF_INET loopback/UDP-backend
cases listed below through the required sequential harness in 2.25 seconds
(4 passed, 3,650 deselected).

The first documentation-only M* head,
`c3210bf54f2116190770c0b95caaa0b9b396e937`, carries the original ledger
update. Later documentation advances the exact pre-landing M* closure head to
`a8cb7995363ebd5177e7e94375abd068e322329f`. Neither head replaces `ca02a40` as
the qualified MegaPad executable revision or adds executable qualification.

Paired A* completion qualified exact Akashic executable code
`4b8680568a229b1bd114d3a05fa4e73f745157ab` against exact MegaPad executable
code `ca02a40c04840791c731dbb7c77ecd7e85eb4909`. The Akashic Checkpoint-5
product journey passed at 27.1 billion guest steps in 811.14 seconds with
stable replay, its read-only Rabbit data plane, and complete teardown; the
current canonical Desktop passed at 13.288 billion steps in 332.51 seconds.
Akashic documentation-only A* head
`c69fbe57cb6169c80560033e94d3d9a640ad9def` records that result without
replacing either tested executable revision. This is paired application
integration evidence, not a change to MegaPad networking code or a new TCP/TLS
claim. Local MegaPad `main` was fast-forwarded to exact pre-landing M* closure
head `a8cb7995363ebd5177e7e94375abd068e322329f`, and local Akashic `main` was
fast-forwarded to exact A* closure head
`c69fbe57cb6169c80560033e94d3d9a640ad9def`. The cached `origin/main` refs,
confirmed fresh during landing preflight, remain MegaPad
`f4b8144786001e423291b9458f24e0efa7ab70ce` and Akashic
`d2e9551ffc37e324bb83acf51108f506599edfd5`; neither repository has been
pushed. The documentation-only landing record containing this update follows
those pre-landing heads and adds no executable qualification. Exact
`ca02a40`/`4b86805` remain the qualified executable pair.

## Historical 2026-08-22 integration reconciliation

Combining this transport line with the dynamic KDOS module registry did not
change TCP/TLS authority, wire behavior, table geometry, ownership, or close
semantics. The registry uses stable Bank-0 allocation and survives
`XMEM-RESET`. Networking source now compiles inside a capacity-derived user
dictionary that is disjoint from general XMEM. BIOS active bounds preflight
every complete HERE-growing operation: exact fit succeeds, and rewind, address
wrap, or overrun throws caught KDOS dictionary fault `-8` before any write.
XMEM floor, free-list, and live high-water checks prevent allocation into the
dictionary span.

The four-core networking source fixture at that checkpoint used a 485M-step
construction allowance. That is a source-loading test-infrastructure correction,
not a TCP, TLS, runtime, or connection-capacity change. `8f0e478` also fixes a general KDOS
`BALANCE` convergence defect for sparse run queues; it does not revise the
transport's historical LAST-ACK scheduler diagnosis.

The historical in-sandbox sequential sweep at `8f0e478` was:

```text
make test-sequential K='not test_udp_backend_lifecycle and not test_udp_backend_roundtrip and not test_nic_device_with_udp_backend and not test_nic_device_backend_rx'
3613 passed, 36 skipped, 4 deselected in 1470.94s
```

`TestKDOSMulticore` separately passed 87/87 in 854.40 seconds.

Eleven other cases in `tests/test_networking.py` pass in the sandbox. The four
deselected cases are host AF_INET loopback/UDP-backend confirmations, not guest
injected-frame UDP failures:

- `tests/test_networking.py::TestNICBackends::test_udp_backend_lifecycle`
- `tests/test_networking.py::TestNICBackends::test_udp_backend_roundtrip`
- `tests/test_networking.py::TestNICBackends::test_nic_device_with_udp_backend`
- `tests/test_networking.py::TestNICBackends::test_nic_device_backend_rx`

Unsandboxed execution was requested and rejected by the approval service at
that historical checkpoint. Final M* closure subsequently passed all four on
the unchanged executable code through the required sequential harness in 2.25
seconds (4 passed, 3,650 deselected).

## Secure-server closure status

The authority substrate now includes the exact authenticated socket-publication
boundary and `TLS-LISTEN`, which copies policy, pins the exact credential, and
publishes its passive TCB atomically while returning its opaque handle and
generation. `TLS-SERVER-ACCEPT-CLAIM` consumes that exact authority and moves
one queued child directly into a prepared TLS context, so no plaintext accepted
socket crosses the secure boundary. The generic `LISTEN` entry remains
fail-closed for TLS descriptors, and `SOCK-ACCEPT` fails closed before consuming
a secure child. Akashic's persistent listener owner instead carries exact
listener and context authority, asks XIO to serialize one bounded accept
request, and invokes one lower operation on each cooperative step. KDOS retains
all credential, wire, authentication, publication, and teardown decisions;
Akashic owns deadline/cancellation precedence, result retention, and cleanup
settlement. Authenticated completion is adopted into the shared established
TLS NIO port, after which HCONN is unchanged. The independent peer journeys now
complete the TCP/TLS handshake, verify the credential chain and hostname,
negotiate ALPN, exchange HTTP bytes, complete `close_notify` and FIN, reuse the
listener, and recover after cancellation, timeout, malformed ClientHello, and
cleanup contention. Remaining transport work is broader profile and
concurrency maturity plus the uint24-maximum Certificate capstone; the
protocol-maximum ClientHello capstone is already complete.

Historically, a 144-byte caller-owned KDOS coordinator exercised the same
lower phases and its six public-path journeys passed in 39.58 seconds. It was a
migration oracle, not the final lifecycle boundary, and was removed after the
Akashic success and recovery paths supplied equivalent composition evidence.
Those measurements remain useful regression history; the coordinator, its
listener lease bookkeeping, and its public entries are not part of the current
KDOS interface.
