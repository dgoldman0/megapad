# Storage faults and the request observer

Tests of filesystem code need a storage device that fails on command: a
write torn partway through a sector, a read that stops at a given sector, a
flush that is refused or silently lost, a medium pulled out midway. They
also need to see what the device was asked to do, in order, and to plant a
fault in the third write or the second read after it. Both of MegaPad's
storage controllers provide this machinery: the emulator's byte-serial
controller (`emulator/devices.py`, `Storage`) and the simulator's checked
service (`simulator/accel/storage_service.h`, which `simulator/storage.py`
exports as `HostedStorageService`). The faults, the
rule that decides which fault acts and what it leaves, and the observer are
defined once, in `shared/accel/storage_faults.h` and `storage_faults.cpp`,
and both native extensions compile them (`_mp64_accel` and
`_megaforth_native`, each with its own copy of the classes). This is test
machinery, not guest policy: no guest word can plant a fault or read the
record.

## Faults

`inject_fault(stage, result=MEDIA_FAILURE, *, command=None,
sector_index=None, byte_index=None, action="fail")` queues one one-shot
fault. A field left as `None` matches every value. The stages name points a
controller reaches in a request:

- `accept` and `start`, before the controller checks and admits the
  request;
- for a READ, for each sector in turn, `media` (the sector is read from the
  medium) and then `dma` once for each of its 512 bytes (the byte is written
  to guest memory);
- for a WRITE, for each sector, `dma` for each byte (read from guest memory)
  and then `media` for each byte (written to the medium), and
  `write_complete` after the last sector;
- for a FLUSH, `flush`, before the medium is made durable.

Sector and byte indices count from zero within one controller command,
which moves at most 255 sectors; the BIOS splits a larger transfer into
several commands. A READ takes each sector from the medium whole, so a
`media` fault with a byte index never meets a READ.

At each point the fault that acts is the first in planting order whose
stage is that point's and whose command, sector index and byte index, where
set, equal the point's. A point without a sector or byte index matches only
a fault that leaves that index unset. The fault is removed when it acts.

The action decides what follows. `fail` completes the command with the
fault's result. `detach` removes the medium, advancing its generation, and
completes with `MEDIA_REMOVED`. `stall` holds the command until a test
releases it, and is allowed only at `accept` and `start`, where releasing it
cannot repeat an effect already applied. A completed command's result is
marked `PARTIAL` when the command had already changed guest memory or the
medium and the result is not `OK`; `TRANSFERRED` counts the sectors
completed before the point. A fault whose result is `OK` therefore models a
lost write tail or a lost flush: the command reports success without
finishing its work, and the BIOS reports a transfer that confirmed fewer
sectors than it asked for as `INTERNAL`.

The same planting rules refuse the same malformed faults on both machines,
with the same messages: an unknown stage or action, a stall at a later
stage, a result or command that does not fit in a byte, a negative index.

## How each controller applies a fault

The emulator's controller moves a request one beat at a time and asks the
fault queue at every point it reaches (`FaultQueue::take`). Its behaviour is
the reference.

The simulator's service completes each request when the guest makes it, so
it asks once, after admission, where in the whole command the first fault
acts (`FaultQueue::take_in_transfer`). The answer is the point the emulator
would have reached first, with what the transfer had done there: for a READ
the bytes already in guest memory, for a WRITE the bytes already on the
medium, and the whole sectors completed. The service lands exactly that
prefix, then completes the command with the result the rule gives and
composes the checked word's result as the BIOS does. Faults at `accept`,
`start` and `flush` are taken at those points. A stall cannot be planted:
nothing can hold a request that completes when it is made, so the service
refuses one, and tests of stalls and controller resets stay with the
emulator.

The simulator's service is itself native code, so the faults and the
observer are part of it. The Python disk words call it, and so do the
engine's own disk words without returning to Python, except while an
observer has a HOOK, which is Python.

## The request observer

`observe(hook=None)` attaches a new observer and returns it. From then on
the controller admits each command once it has accepted and checked it,
just before the transfer or flush begins: the emulator for each controller
command, the simulator for each controller-sized chunk of a transfer and
for each flush, so both record the same requests. At admission the observer
records `(command, lba, count)`, with a flush naming no sectors as
`(0xFF, 0, 0)`, plants the faults planned for that request, and then calls
HOOK, if one was given, with the same three values. Faults planted at
admission act within that request.

`plan_fault(request_command, ordinal, *, after_writes=None, **fault)`
plans a fault, described as `inject_fault` takes one, for the ORDINAL-th
admitted request of REQUEST_COMMAND, counting from one. With AFTER_WRITES
the ordinal counts only the requests admitted after exactly that many
writes, so `plan_fault(READ, 2, after_writes=3, stage="media")` fails the
second read after the third write. A fault can be planned only while an
observer is attached, since the observer plants it.

The observer reports `requests`, the counts `reads`, `writes` and
`flushes`, and `pending`, the planned faults whose request has not come.
`stop_observing()` detaches it; the record stays readable.

## Why the definition is native

The rule and the observer are C++ so that the simulator engine's disk
words, which run the whole storage service in native code, apply them
without returning to Python. The emulator's controller is Python and calls
them through its accelerator; with no fault queued its check is one flag
test, so its byte-serial path costs what it did. On the simulator, a block
read through KDOS's `BD-READ` cost 76 to 82 microseconds on 2026-11-16
while the disk words ran in Python (MegaPad 977ef95e), almost all of it the
engine's returns to Python for `UNIT-DISK@`, `UNIT-DISK-MEDIA-GEN` and the
transfer word. With the words in the engine, on the same machine, a
two-sector `BD-READ` costs 0.90 microseconds and a `BD-WRITE` 1.51,
with no return to Python.

## Tests

`tests/test_storage_contract.py` runs its guest-visible tests on both
machines: the same BIOS words, planted faults and observer on an emulator
machine and on a simulator runtime, with the same results, guest memory,
medium, host file and request record required of both. Its register-level
tests stay with the emulator, whose controller alone has registers.
