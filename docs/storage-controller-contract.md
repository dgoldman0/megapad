# Storage controller and checked block-I/O contract

This document freezes the observable Megapad storage contract.  It applies
equally to the host emulator and the RTL-visible controller.  The original
registers and commands remain compatible; the appended generation-guarded
submit registers let persistent block-device handles fail closed across
replacement media.  Raw register words remain available for diagnostics and
boot compatibility, but production filesystem code uses checked BIOS
operations.

The controller transfers fixed 512-byte logical sectors.  It accepts one
request at a time and has no request queue.  A successful `WRITE` means the
entire requested transfer was accepted by the live backend.  It is not a
durability claim.  A subsequent successful `FLUSH` is the ordering and
durability boundary for all earlier successful writes.

## Register map

The original offsets through `TOTAL_SECTORS` retain their byte meanings.  The
result registers occupy previously unused bytes in the same storage MMIO
window.

| Register | Offset | Access | Meaning |
| --- | --- | --- | --- |
| `CMD` | `+0x00` | W | `01` READ, `02` WRITE, `03` status no-op, `04` RESET, `FF` FLUSH |
| `STATUS` | `+0x01` | R/W1C | Live and sticky state described below |
| `SECTOR` | `+0x02..+0x05` | RW | Starting LBA, unsigned u32 little-endian |
| `DMA_ADDR` | `+0x06..+0x0d` | RW | First DMA byte address, unsigned u64 little-endian |
| `SEC_COUNT` | `+0x0e` | RW | Controller request count; legal values are 1 through 255 |
| `DATA` | `+0x0f` | RW | Legacy diagnostic PIO byte port; not part of the qualified checked path |
| `DMA_PUSH` | `+0x10` | W | Legacy byte-serial DMA address writer |
| `TOTAL_SECTORS` | `+0x11..+0x14` | R | Current attached capacity, u32 little-endian; zero while detached |
| `RESULT` | `+0x15` | R | Last accepted request result; bit 7 is `PARTIAL`, bits 0..6 are the cause |
| `COMPLETE` | `+0x16..+0x19` | R | Terminal-completion generation, wrapping u32 little-endian |
| `MEDIA_GEN` | `+0x1a..+0x1d` | R | Attachment identity generation, wrapping u32 little-endian |
| `CAPS` | `+0x1e` | R | Backend capability bits |
| `TRANSFERRED` | `+0x1f` | R | Completely transferred sectors in the last accepted controller request |
| `EXPECTED_MEDIA_GEN` | `+0x20..+0x23` | RW | Required attachment generation for `GUARDED_CMD`, wrapping u32 little-endian |
| `GUARDED_CMD` | `+0x24` | W | Conditionally submit READ, WRITE, or FLUSH; reads return zero |

`STATUS` has these bits:

- bit 0, `BUSY`: one accepted request is in flight;
- bit 1, `ERROR`: the last published `RESULT` cause is nonzero;
- bit 2, `REJECTED`: a command or setup-register write was attempted while
  busy; sticky and cleared by writing one to this bit;
- bit 3, `RESULT_VALID`: `RESULT`, `TRANSFERRED`, and the current `COMPLETE`
  value describe a terminal accepted request;
- bit 4, `MEDIA_CHANGED`: attach, detach, or swap occurred; sticky and cleared
  by writing one to this bit;
- bit 5, `WRITE_PROTECTED`: the current backend refuses writes;
- bit 7, `PRESENT`: media is attached.

Writing `STATUS` only clears written-one bits 2 and 4.  It cannot clear a
terminal result or alter live state.

`CAPS` uses bit 0 for READ, bit 1 for WRITE, bit 2 for FLUSH, bit 3 for precise
results, bit 4 for the completion generation, bit 5 for the media generation,
and bit 6 for atomic generation-guarded submission.  Unassigned bits read
zero.

## Acceptance, completion, and ordering

An idle READ or WRITE command atomically snapshots `SECTOR`, `DMA_ADDR`,
`SEC_COUNT`, and `MEDIA_GEN`.  An idle FLUSH snapshots the media identity but
ignores transfer setup.  `BUSY` becomes part of the accepted state before the
command write is acknowledged.  Later setup writes cannot redirect an active
request.

For a persistent device or volume handle, software writes its captured
generation to `EXPECTED_MEDIA_GEN` and submits READ, WRITE, or FLUSH through
`GUARDED_CMD`.  Generation comparison and command acceptance are one
controller action.  If the expected value differs from the generation that is
current at that acceptance edge, the controller publishes `MEDIA_REMOVED`
with `TRANSFERRED=0` and increments `COMPLETE` without DMA, media protocol,
media mutation, or FLUSH effects.  A matching guarded command follows the
same validation and execution path as its `CMD` counterpart.  Other values
written to `GUARDED_CMD`, including STATUS and RESET, publish `UNSUPPORTED`.

The controller publishes `RESULT` and `TRANSFERRED`, increments `COMPLETE`
exactly once, sets `RESULT_VALID` and `ERROR` as appropriate, and finally
leaves the busy state as one terminal publication.  Software waits for
`COMPLETE` to differ from the pre-command snapshot; observing idle alone is
not proof that its request completed.  Wrapping comparison is by inequality,
not numeric ordering.

Validation failure is an accepted, immediate terminal request and therefore
increments `COMPLETE`.  An unsupported idle command behaves the same way with
`UNSUPPORTED`.  Command `03` remains a compatibility no-op and does not
publish a completion.

While busy, writes to the programmable/diagnostic range `+0x02..+0x10`, the
guard setup range `+0x20..+0x23`, `GUARDED_CMD`, and commands other than RESET
are ignored and set `REJECTED`; they neither change the active snapshot nor
increment `COMPLETE`.  `STATUS` W1C remains usable, and writes to the
read-only result range are harmless no-ops.
RESET aborts an active request, prevents later actions from that request, and
publishes one `RESET_ABORTED` completion.  An idle RESET clears the current
terminal/rejection state without incrementing `COMPLETE`; it does not clear
`MEDIA_CHANGED` or change media contents.  Active and idle RESET restore
`EXPECTED_MEDIA_GEN` with the rest of the programmable request tuple.

Requests from production software are serialized across register setup,
command submission, completion, and result readback.  The checked layer uses
the machine-wide filesystem spinlock.  Controller busy rejection remains
defined defense in depth, not a substitute for that lock.

The checked layer owns lock 2 itself.  Callers must not wrap a checked disk or
filesystem operation in `FS-ACQUIRE` or `WITH-LOCK ... FS-LOCK`: the hardware
lock recognizes the same core as its owner but does not maintain a recursion
depth, so the inner checked operation's release would prematurely release the
outer critical section.

## Validation and results

Before any DMA or media mutation, READ and WRITE validate:

- attached media and command support;
- a nonzero count;
- `sector < total` and `count <= total - sector`;
- the inclusive last DMA byte, `dma + count * 512 - 1`, does not overflow
  u64; and
- one complete reachable DMA span inside a single physical memory window.

The byte bus has alignment one.  Backends with a stricter alignment rule must
advertise and validate it before command acceptance; they may not silently
round an address.

`RESULT` bit 7 means that an externally visible prefix may have crossed the
DMA or media boundary.  `TRANSFERRED` counts only whole sectors.  A failed
sector is not included.  A zero-partial validation failure must not read guest
memory, mutate guest memory, change media bytes, or grow a host image.

| Cause | Value | Meaning |
| --- | ---: | --- |
| `OK` | `00` | Complete success |
| `NO_MEDIA` | `01` | No attached media |
| `UNSUPPORTED` | `02` | Unknown command or unsupported operation |
| `INVALID_COUNT` | `03` | Sector count is zero or otherwise invalid |
| `LBA_RANGE` | `04` | Requested interval is outside the attached capacity |
| `ADDRESS_OVERFLOW` | `05` | DMA byte interval wraps u64 |
| `DMA_INVALID` | `06` | DMA span is unmapped, crosses regions, or violates alignment |
| `DMA_FAILURE` | `07` | Accepted DMA transaction failed or never acknowledged |
| `WRITE_PROTECTED` | `08` | Backend refuses mutation |
| `MEDIA_FAILURE` | `09` | Media protocol or host I/O failed |
| `TIMEOUT` | `0a` | Controller or checked-software completion bound expired |
| `MEDIA_REMOVED` | `0b` | Accepted request lost its snapshotted media identity |
| `RESET_ABORTED` | `0c` | RESET terminated the accepted request |
| `FLUSH_FAILURE` | `0d` | Durability operation failed |
| `INTERNAL` | `0e` | Detected controller invariant failure |

Busy rejection is represented by `STATUS.REJECTED` while the active request
retains ownership of the terminal result registers.  Checked software that
cannot acquire the serialization lock within its bound returns `TIMEOUT`
without issuing a command.

## Media identity and durability

Attach, detach, and swap increment `MEDIA_GEN`, including wrap from all ones
to zero.  An accepted request remains bound to the snapshotted generation.
Removal or replacement aborts it with `MEDIA_REMOVED`; it must never continue
against replacement bytes.  Block-device handles retain this generation and
use guarded submission so a request that begins after replacement also fails
before touching the new attachment.

A media-identity event does not itself rewrite `SECTOR`, `DMA_ADDR`,
`SEC_COUNT`, `EXPECTED_MEDIA_GEN`, or the `DMA_PUSH` serializer state, and it
does not erase an idle terminal publication.  If a request is active, its
`MEDIA_REMOVED` completion becomes the new terminal publication.  RESET
remains the explicit operation that restores the programmable request tuple
and clears terminal state.

For the host emulator, successful FLUSH means the complete live image was
written, the host file was flushed, and `fsync` returned successfully.  A
session-close convenience save is not a guest durability operation and does
not qualify this boundary.

For an SD backend, WRITE success requires accepted data-response status and
the card leaving its busy state for every transferred sector.  FLUSH waits for
all earlier writes, obtains successful card status, and observes ready state.
This promises completed card protocol operations; it does not claim survival
from storage hardware that lies after acknowledging them.

## Checked BIOS/KDOS interface

Production callers use:

```forth
DISK-READ-CHECKED   ( dma lba count -- completed status )
DISK-WRITE-CHECKED  ( dma lba count -- completed status )
DISK-FLUSH-CHECKED  ( -- status )
```

The checked transfer words validate the complete request, own serialization,
split counts larger than 255, stop at the first failing chunk, use bounded
completion waits, and return only confirmed whole sectors.  `status` is the
cause value above with the partial bit preserved when supplied by the
controller.  A successful multi-chunk result has `completed = count` and
`status = OK`.

Legacy `DISK-SEC!`, `DISK-DMA!`, `DISK-N!`, `DISK-READ`, `DISK-WRITE`, and
`DISK-FLUSH` remain diagnostic compatibility words.  Filesystem production
paths may not implement their own polling or result translation.

## Qualification boundary

Deterministic emulator and RTL test facilities may inject rejection, selected
DMA/media failure, stalled acknowledgement, write completion failure, flush
failure, timeout, and removal.  These controls are test machinery and are not
guest policy or production filesystem APIs.

Qualification of generation-bound handles additionally proves that stale
guarded READ, WRITE, and FLUSH commands complete as `MEDIA_REMOVED` with zero
progress and cause no DMA, replacement-media, or durability side effect.

The first contract is complete only when the emulator, focused RTL controller
tests, BIOS/KDOS checked words, and an MP64FS cold-process regression agree on
the behaviors above.  The regression must perform guest WRITE and FLUSH,
destroy the process without relying on session-close autosave, reopen the host
image in a fresh process, and verify the exact committed bytes.
