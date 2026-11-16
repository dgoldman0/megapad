"""Native disk words against the Python dispatcher.

DISK@, DISK-SECTORS, DISK-MEDIA-GEN, DISK-CAPS, their UNIT-DISK-* forms and
the checked READ, WRITE and FLUSH words run in the native executor while the
runtime keeps the storage services, memory and spinlock bank the program
was bound to.  The service is native code that the Python words call too
(simulator/accel/storage_service.h).  Each case compares the complete
observation of the Python and native executors, guest memory included, with
every unit's registers and medium and the request observer's record.
"""

from __future__ import annotations

import pytest


pytest.importorskip("_megaforth_native")

from shared.storage import (  # noqa: E402
    SECTOR_SIZE,
    STORAGE_CMD_FLUSH,
    STORAGE_CMD_READ,
    STORAGE_CMD_WRITE,
    STORAGE_RESULT_ADDRESS_OVERFLOW,
    STORAGE_RESULT_DMA_INVALID,
    STORAGE_RESULT_INVALID_COUNT,
    STORAGE_RESULT_LBA_RANGE,
    STORAGE_RESULT_MEDIA_FAILURE,
    STORAGE_RESULT_MEDIA_REMOVED,
    STORAGE_RESULT_NO_MEDIA,
    STORAGE_RESULT_OK,
)
from simulator.memory import AddressClass  # noqa: E402
from simulator.platform import create_one_core_address_space  # noqa: E402
from simulator.runtime import MegaForthRuntime  # noqa: E402
from simulator.storage import HostedStorageService  # noqa: E402
from tests.simulator.test_native_execution import _observe  # noqa: E402

_SETUP = b"CREATE BUF 4096 ALLOT "


def _medium(sectors: int, salt: int = 0) -> bytes:
    return bytes(
        (index * 7 + salt * 13 + 3) & 0xFF for index in range(sectors * SECTOR_SIZE)
    )


# The boot disk, two units with no medium and a small user disk.
_UNITS = (_medium(64), None, None, _medium(8, salt=1))


def _runtimes(source: bytes, *, units=_UNITS, external_size: int = 0, prepare=None,
              service=HostedStorageService):
    runtimes = []
    for backend in ("python", "native"):
        services = [None if medium is None else service(medium) for medium in units]
        runtime = MegaForthRuntime(
            execution_backend=backend,
            memory=create_one_core_address_space(external_size=external_size),
            storage=services[0], settings_storage=services[1],
            system_storage=services[2], user_storage=services[3],
        )
        runtime.evaluate(_SETUP + source, source_name="disk-words.f")
        runtime.prepared = None if prepare is None else prepare(runtime)
        runtimes.append(runtime)
    return runtimes


def _units(runtime):
    return tuple(
        (unit.status, unit.total_sectors, unit.media_generation, unit.completion,
         unit.result, unit.transferred, unit.image_bytes)
        for unit in runtime.storage_units
    )


def _compare(runtimes, *, word="RUN", spans=None, inputs=(), require_native=True):
    if spans is None:
        spans = ((runtimes[0].find(b"BUF").body_address, 4096),)
    observed = [
        _observe(runtime, word, spans=spans, inputs=inputs) for runtime in runtimes
    ]
    assert observed[1][0] == observed[0][0]
    assert _units(runtimes[1]) == _units(runtimes[0])
    if require_native:
        assert observed[1][1] > 0, "the disk words must run natively"
    return observed[0][0]


def test_the_registers_of_every_unit_and_of_a_missing_one_match() -> None:
    observation = _compare(_runtimes(
        b": RUN DISK@ DISK-SECTORS DISK-MEDIA-GEN DISK-CAPS "
        b"5 0 DO I UNIT-DISK@ I UNIT-DISK-SECTORS I UNIT-DISK-MEDIA-GEN "
        b"I UNIT-DISK-CAPS LOOP ;"
    ))
    assert observation["error"] is None
    assert len(observation["data"]) == 24


def test_reads_and_writes_of_every_shape_and_outcome_match() -> None:
    observation = _compare(_runtimes(
        b": RUN BUF 3 2 DISK-READ-CHECKED BUF 1024 + 9 1 DISK-WRITE-CHECKED "
        b"BUF 4 1 DISK-MEDIA-GEN DISK-READ-GEN-CHECKED "
        b"BUF 512 + 5 1 DISK-MEDIA-GEN DISK-WRITE-GEN-CHECKED "
        b"BUF 2048 + 0 2 3 UNIT-DISK-MEDIA-GEN 3 UNIT-DISK-READ-GEN-CHECKED "
        b"BUF 1 1 3 UNIT-DISK-MEDIA-GEN 3 UNIT-DISK-WRITE-GEN-CHECKED "
        b"BUF 0 1 0 1 UNIT-DISK-READ-GEN-CHECKED "
        b"BUF 0 1 99 3 UNIT-DISK-READ-GEN-CHECKED "
        b"BUF 0 1 0 7 UNIT-DISK-READ-GEN-CHECKED "
        b"BUF 0 0 DISK-READ-CHECKED BUF 63 2 DISK-READ-CHECKED "
        b"-1 0 1 DISK-READ-CHECKED 2097152 0 1 DISK-READ-CHECKED ;"
    ))
    assert observation["error"] is None
    assert observation["data"][1::2] == (STORAGE_RESULT_OK,) * 6 + (
        STORAGE_RESULT_NO_MEDIA, STORAGE_RESULT_MEDIA_REMOVED, STORAGE_RESULT_NO_MEDIA,
        STORAGE_RESULT_INVALID_COUNT, STORAGE_RESULT_LBA_RANGE,
        STORAGE_RESULT_ADDRESS_OVERFLOW, STORAGE_RESULT_DMA_INVALID,
    )


def test_a_transfer_of_several_controller_commands_matches() -> None:
    # 300 sectors take two commands of at most 255 sectors each.
    runtimes = _runtimes(
        b": RUN EXT-MEM-BASE 10 300 DISK-READ-CHECKED "
        b"EXT-MEM-BASE 512 + 320 300 DISK-WRITE-CHECKED ;",
        units=(_medium(640), None, None, None),
        external_size=1 << 20,
    )
    (base,) = (spec.base for spec in runtimes[0].memory.regions
               if spec.kind is AddressClass.EXTERNAL)
    observation = _compare(runtimes, spans=((base, 301 * SECTOR_SIZE),))
    assert observation["data"] == (300, 0, 300, 0)
    # The write starts a sector into what the read left, so its last sector
    # is memory the read never reached.
    assert runtimes[1].storage.image_bytes[320 * SECTOR_SIZE:620 * SECTOR_SIZE] == (
        _medium(640)[11 * SECTOR_SIZE:310 * SECTOR_SIZE] + bytes(SECTOR_SIZE)
    )


def test_a_read_into_an_absent_page_is_seen_by_the_same_run() -> None:
    # The pages under the destination do not exist before the read, so the
    # run must not keep its earlier look at them.
    runtimes = _runtimes(
        b": RUN EXT-MEM-BASE 12288 + DUP C@ SWAP DUP 2 1 DISK-READ-CHECKED "
        b"2DROP DUP C@ SWAP 511 + C@ ;",
        external_size=1 << 20,
    )
    observation = _compare(runtimes, spans=())
    assert observation["error"] is None
    medium = _medium(64)
    assert observation["data"] == (0, medium[1024], medium[1535])


def test_faults_and_the_request_record_match() -> None:
    def prepare(runtime):
        storage = runtime.storage
        storage.inject_fault("media", command=STORAGE_CMD_WRITE, sector_index=1,
                             byte_index=100)
        observer = storage.observe()
        storage.plan_fault(STORAGE_CMD_READ, 2, stage="dma",
                           result=STORAGE_RESULT_MEDIA_FAILURE, byte_index=9)
        return observer

    runtimes = _runtimes(
        b": RUN BUF 4 1 DISK-READ-CHECKED BUF 5 3 DISK-WRITE-CHECKED "
        b"BUF 6 2 DISK-READ-CHECKED BUF 7 1 DISK-READ-CHECKED ;",
        prepare=prepare,
    )
    observation = _compare(runtimes)
    assert observation["error"] is None
    assert observation["data"][1::2] != (STORAGE_RESULT_OK,) * 4
    records = [
        (runtime.prepared.requests, runtime.prepared.reads, runtime.prepared.writes,
         len(runtime.storage._faults))
        for runtime in runtimes
    ]
    assert records[1] == records[0]
    assert records[0][0] == (
        (STORAGE_CMD_READ, 4, 1), (STORAGE_CMD_WRITE, 5, 3),
        (STORAGE_CMD_READ, 6, 2), (STORAGE_CMD_READ, 7, 1),
    )


def test_flushes_of_every_shape_and_their_host_files_match(tmp_path) -> None:
    def prepare(runtime):
        path = tmp_path / f"{runtime.execution_backend}.img"
        path.write_bytes(_medium(64))
        runtime.storage.attach(_medium(64), image_path=path, file_holds_media=True)
        observer = runtime.storage.observe()
        runtime.storage.plan_fault(STORAGE_CMD_FLUSH, 3, stage="flush",
                                   result=STORAGE_RESULT_MEDIA_FAILURE)
        return path, observer

    runtimes = _runtimes(
        b": RUN BUF 3 2 DISK-WRITE-CHECKED 2DROP DISK-FLUSH-CHECKED "
        b"BUF 9 1 DISK-WRITE-CHECKED 2DROP DISK-MEDIA-GEN DISK-FLUSH-GEN-CHECKED "
        b"BUF 12 1 DISK-WRITE-CHECKED 2DROP DISK-MEDIA-GEN DISK-FLUSH-GEN-CHECKED "
        b"99 DISK-FLUSH-GEN-CHECKED 3 UNIT-DISK-MEDIA-GEN 3 UNIT-DISK-FLUSH-GEN-CHECKED "
        b"0 1 UNIT-DISK-FLUSH-GEN-CHECKED 0 7 UNIT-DISK-FLUSH-GEN-CHECKED ;",
        prepare=prepare,
    )
    observation = _compare(runtimes)
    assert observation["data"] == (
        STORAGE_RESULT_OK, STORAGE_RESULT_OK, STORAGE_RESULT_MEDIA_FAILURE,
        STORAGE_RESULT_MEDIA_REMOVED, STORAGE_RESULT_OK, STORAGE_RESULT_NO_MEDIA,
        STORAGE_RESULT_NO_MEDIA,
    )
    (python_file, python_observer), (native_file, native_observer) = (
        runtime.prepared for runtime in runtimes
    )
    assert native_observer.requests == python_observer.requests
    assert native_observer.flushes == 3
    held = native_file.read_bytes()
    assert held == python_file.read_bytes()
    # The failed flush left the last write on the medium alone.
    expected = bytearray(_medium(64))
    expected[3 * SECTOR_SIZE:5 * SECTOR_SIZE] = bytes(2 * SECTOR_SIZE)
    expected[9 * SECTOR_SIZE:10 * SECTOR_SIZE] = bytes(SECTOR_SIZE)
    assert held == expected


def test_a_write_protected_and_a_detached_unit_match() -> None:
    def prepare(runtime):
        runtime.user_storage.set_write_protected(True)

    runtimes = _runtimes(
        b": RUN BUF 0 1 3 UNIT-DISK-MEDIA-GEN 3 UNIT-DISK-WRITE-GEN-CHECKED 3 UNIT-DISK@ ; "
        b": AFTER BUF 0 1 DISK-READ-CHECKED DISK@ DISK-SECTORS ;",
        prepare=prepare,
    )
    assert _compare(runtimes)["error"] is None
    for runtime in runtimes:
        runtime.storage.detach()
    assert _compare(runtimes, word="AFTER")["error"] is None


def test_a_hook_a_held_lock_or_the_return_stack_leaves_a_transfer_to_python() -> None:
    def hooked(runtime):
        admitted = []
        runtime.storage.observe(lambda *request: admitted.append(request))
        return admitted

    runtimes = _runtimes(b": RUN BUF 4 1 DISK-READ-CHECKED ;", prepare=hooked)
    assert _compare(runtimes, require_native=False)["error"] is None
    assert runtimes[1].prepared == [(STORAGE_CMD_READ, 4, 1)]

    def locked(runtime):
        assert runtime.spinlocks.acquire(2, 0) == 0

    runtimes = _runtimes(b": RUN BUF 4 1 DISK-READ-CHECKED DISK-FLUSH-CHECKED ;",
                         prepare=locked)
    observation = _compare(runtimes, require_native=False)
    # The words take the lock their own core holds and release it, as the
    # BIOS's do.
    assert observation["data"] == (1, 0, 0)
    assert all(runtime.spinlocks.owner(2) is None for runtime in runtimes)

    runtimes = _runtimes(b": RUN 4 1 DISK-READ-CHECKED ;")
    floor = runtimes[0].main_context.returns.floor
    observation = _compare(runtimes, spans=((floor, SECTOR_SIZE),), inputs=(floor,),
                           require_native=False)
    assert observation["data"] == (1, 0)
    assert observation["memory"] == (_medium(64)[4 * SECTOR_SIZE:5 * SECTOR_SIZE],)


def test_a_subclass_with_its_own_seam_keeps_the_words_in_python() -> None:
    accepted = []

    class Watched(HostedStorageService):
        def _before_guarded_accept(self, command, expected_generation):
            accepted.append((command, expected_generation))

    runtimes = _runtimes(b": RUN BUF 4 1 DISK-READ-CHECKED ;", service=Watched)
    assert runtimes[1]._native_execution.disk_primitives == {}
    observation = _compare(runtimes, require_native=False)
    assert observation["data"] == (1, 0)
    generation = runtimes[0].storage.media_generation
    assert accepted == [(STORAGE_CMD_READ, generation)] * 2
