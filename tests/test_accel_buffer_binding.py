"""Safety contract for Python buffers borrowed by the native accelerator.

The accelerator treats each attached object as a flat byte-addressed memory
region.  These tests pin the lifetime, layout, capacity, and replacement
semantics that keep those native pointers valid.
"""

from __future__ import annotations

import array
import gc
import threading
import time
import weakref
from dataclasses import dataclass

import pytest

import _mp64_accel
from accel_wrapper import Megapad64


UINT64_MAX = (1 << 64) - 1
REGION_BASE = 0x1000


@dataclass(frozen=True)
class Region:
    name: str
    native_attach: str
    size_attr: str
    base_attr: str | None = None
    wrapper_attach: str | None = None
    wrapper_owner: str | None = None


REGIONS = (
    Region("main", "attach_mem", "mem_size"),
    Region(
        "hbw",
        "attach_hbw_mem",
        "hbw_size",
        "hbw_base",
        "attach_hbw",
        "_hbw_buf",
    ),
    Region(
        "ext_mem",
        "attach_ext_mem",
        "ext_mem_size",
        "ext_mem_base",
        "attach_ext_mem",
        "_ext_mem_buf",
    ),
    Region(
        "vram",
        "attach_vram",
        "vram_size",
        "vram_base",
        "attach_vram",
        "_vram_buf",
    ),
)
MAPPED_REGIONS = tuple(region for region in REGIONS if region.base_attr)


def _attach_native(
    state,
    region: Region,
    buf,
    size: int,
    *,
    base: int = REGION_BASE,
) -> None:
    attach = getattr(state, region.native_attach)
    if region.base_attr is None:
        attach(buf, size)
    else:
        attach(buf, base, size)


@pytest.mark.parametrize("region", REGIONS, ids=lambda region: region.name)
def test_attachment_pins_export_until_replaced_and_destroyed(region: Region):
    state = _mp64_accel.CPUState()
    first = bytearray(16)
    _attach_native(state, region, first, len(first))

    with pytest.raises(BufferError):
        first.extend(b"x")

    replacement = bytearray(16)
    _attach_native(state, region, replacement, len(replacement))

    first.extend(b"x")
    assert len(first) == 17
    with pytest.raises(BufferError):
        replacement.extend(b"x")

    del state
    gc.collect()

    replacement.extend(b"x")
    assert len(replacement) == 17


@pytest.mark.parametrize("region", REGIONS, ids=lambda region: region.name)
def test_attachment_retains_exporter_without_a_wrapper_reference(region: Region):
    state = _mp64_accel.CPUState()
    exported = array.array("B", range(16))
    exported_ref = weakref.ref(exported)

    _attach_native(state, region, exported, len(exported))
    del exported
    gc.collect()

    assert exported_ref() is not None

    _attach_native(state, region, bytearray(16), 16)
    gc.collect()

    assert exported_ref() is None


@pytest.mark.parametrize("region", REGIONS, ids=lambda region: region.name)
def test_readonly_replacement_preserves_existing_mapping(region: Region):
    state = _mp64_accel.CPUState()
    original = bytearray(16)
    _attach_native(state, region, original, 8)
    original_size = getattr(state, region.size_attr)
    original_base = (
        getattr(state, region.base_attr) if region.base_attr is not None else None
    )

    with pytest.raises(BufferError):
        _attach_native(
            state,
            region,
            bytes(16),
            16,
            base=REGION_BASE + 0x100,
        )

    assert getattr(state, region.size_attr) == original_size
    if region.base_attr is not None:
        assert getattr(state, region.base_attr) == original_base
    with pytest.raises(BufferError):
        original.extend(b"x")


@pytest.mark.parametrize("region", REGIONS, ids=lambda region: region.name)
def test_attachment_rejects_size_beyond_export_capacity(region: Region):
    state = _mp64_accel.CPUState()
    original = bytearray(16)
    _attach_native(state, region, original, 8)
    original_size = getattr(state, region.size_attr)
    original_base = (
        getattr(state, region.base_attr) if region.base_attr is not None else None
    )

    with pytest.raises(ValueError):
        _attach_native(
            state,
            region,
            bytearray(7),
            8,
            base=REGION_BASE + 0x100,
        )

    assert getattr(state, region.size_attr) == original_size
    if region.base_attr is not None:
        assert getattr(state, region.base_attr) == original_base


def _positive_stride_view():
    return memoryview(bytearray(16))[::2]


def _negative_stride_view():
    return memoryview(bytearray(16))[::-1]


@pytest.mark.parametrize("region", REGIONS, ids=lambda region: region.name)
@pytest.mark.parametrize(
    ("view_factory", "size"),
    (
        pytest.param(_positive_stride_view, 8, id="positive-stride"),
        pytest.param(_negative_stride_view, 16, id="negative-stride"),
    ),
)
def test_attachment_rejects_noncontiguous_views(
    region: Region,
    view_factory,
    size: int,
):
    state = _mp64_accel.CPUState()

    with pytest.raises(ValueError):
        _attach_native(state, region, view_factory(), size)


@pytest.mark.parametrize("region", REGIONS, ids=lambda region: region.name)
def test_attachment_requires_a_one_dimensional_byte_buffer(region: Region):
    state = _mp64_accel.CPUState()
    word_buffer = array.array("I", [0, 0, 0, 0])
    matrix_buffer = memoryview(bytearray(16)).cast("B", shape=(4, 4))

    with pytest.raises(ValueError):
        _attach_native(state, region, word_buffer, word_buffer.itemsize * len(word_buffer))
    with pytest.raises(ValueError):
        _attach_native(state, region, matrix_buffer, matrix_buffer.nbytes)


def test_main_memory_rejects_zero_size():
    state = _mp64_accel.CPUState()

    with pytest.raises(ValueError):
        state.attach_mem(bytearray(), 0)


@pytest.mark.parametrize(
    "region",
    MAPPED_REGIONS,
    ids=lambda region: region.name,
)
def test_unattached_mapped_region_allows_only_zero_size(region: Region):
    state = _mp64_accel.CPUState()

    setattr(state, region.size_attr, 0)
    assert getattr(state, region.size_attr) == 0

    with pytest.raises(ValueError):
        setattr(state, region.size_attr, 1)
    assert getattr(state, region.size_attr) == 0


@pytest.mark.parametrize(
    "region",
    MAPPED_REGIONS,
    ids=lambda region: region.name,
)
def test_attachment_rejects_wrapping_guest_region(region: Region):
    state = _mp64_accel.CPUState()

    with pytest.raises(ValueError):
        _attach_native(
            state,
            region,
            bytearray(16),
            16,
            base=UINT64_MAX - 7,
        )


@pytest.mark.parametrize("region", REGIONS, ids=lambda region: region.name)
def test_public_size_setter_cannot_exceed_export_capacity(region: Region):
    state = _mp64_accel.CPUState()
    _attach_native(state, region, bytearray(16), 8)

    setattr(state, region.size_attr, 12)
    assert getattr(state, region.size_attr) == 12

    with pytest.raises(ValueError):
        setattr(state, region.size_attr, 17)
    assert getattr(state, region.size_attr) == 12

    if region.base_attr is None:
        with pytest.raises(ValueError):
            setattr(state, region.size_attr, 0)
        assert getattr(state, region.size_attr) == 12


@pytest.mark.parametrize(
    "region",
    MAPPED_REGIONS,
    ids=lambda region: region.name,
)
def test_public_base_setter_rejects_wrapping_guest_region(region: Region):
    state = _mp64_accel.CPUState()
    _attach_native(state, region, bytearray(8), 8)

    # A region may end exactly at UINT64_MAX; only wrapping past it is invalid.
    safe_base = UINT64_MAX - 7
    setattr(state, region.base_attr, safe_base)
    assert getattr(state, region.base_attr) == safe_base

    with pytest.raises(ValueError):
        setattr(state, region.base_attr, safe_base + 1)
    assert getattr(state, region.base_attr) == safe_base


def test_wrapper_main_memory_replacement_is_transactional():
    cpu = Megapad64(mem_size=16)
    original = cpu.mem

    with pytest.raises(BufferError):
        cpu.mem = bytes(8)

    assert cpu.mem is original
    assert cpu.mem_size == 16
    assert cpu._cs.mem_size == 16

    replacement = bytearray(32)
    cpu.mem = replacement

    assert cpu.mem is replacement
    assert cpu.mem_size == 32
    assert cpu._cs.mem_size == 32


@pytest.mark.parametrize(
    "region",
    MAPPED_REGIONS,
    ids=lambda region: region.name,
)
def test_wrapper_region_replacement_is_transactional(region: Region):
    cpu = Megapad64(mem_size=16)
    original = bytearray(16)
    getattr(cpu, region.wrapper_attach)(original, REGION_BASE, 8)

    with pytest.raises(BufferError):
        getattr(cpu, region.wrapper_attach)(
            bytes(16),
            REGION_BASE + 0x100,
            16,
        )

    assert getattr(cpu, region.wrapper_owner) is original
    assert getattr(cpu._cs, region.base_attr) == REGION_BASE
    assert getattr(cpu._cs, region.size_attr) == 8


def test_attachment_and_metadata_mutation_fail_fast_during_execution():
    state = _mp64_accel.CPUState()
    program = bytearray([0x91])  # OUT1; callback runs inside step_one().
    replacement = bytearray([0x00])
    _attach_native(state, REGIONS[0], program, len(program))
    state.psel = 3
    state.xsel = 2
    state.set_reg(3, 0)
    state.set_reg(2, 0)
    rejected = []

    def on_output(_port, _value):
        try:
            state.attach_mem(replacement, len(replacement))
        except RuntimeError:
            rejected.append("attachment")

        try:
            state.mem_size = len(program)
        except RuntimeError:
            rejected.append("metadata")

        try:
            state.nic_sync_mem_ptrs()
        except RuntimeError:
            rejected.append("NIC pointer sync")

    result = _mp64_accel.run_steps(
        state,
        mmio_read8=lambda _addr: 0,
        mmio_write8=lambda _addr, _value: None,
        on_output=on_output,
        csr_read_override=None,
        mmio_start=0xFFFF_FF00_0000_0000,
        mmio_end=0xFFFF_FF80_0000_0000,
        max_steps=1,
    )

    assert result.steps_executed == 1
    assert rejected == ["attachment", "metadata", "NIC pointer sync"]


def test_nested_execution_on_one_state_fails_fast():
    state = _mp64_accel.CPUState()
    program = bytearray([0x91])  # OUT1; callback runs inside step_one().
    state.attach_mem(program, len(program))
    state.psel = 3
    state.xsel = 2
    state.set_reg(3, 0)
    state.set_reg(2, 0)
    rejected = []

    def nested_output(_port, _value):
        with pytest.raises(RuntimeError, match="already executing"):
            _mp64_accel.step_one(
                state,
                mmio_read8=lambda _addr: 0,
                mmio_write8=lambda _addr, _value: None,
                on_output=lambda _port, _value: None,
                csr_read_override=None,
                mmio_start=0xFFFF_FF00_0000_0000,
                mmio_end=0xFFFF_FF80_0000_0000,
            )
        rejected.append("nested execution")

    result = _mp64_accel.run_steps(
        state,
        mmio_read8=lambda _addr: 0,
        mmio_write8=lambda _addr, _value: None,
        on_output=nested_output,
        csr_read_override=None,
        mmio_start=0xFFFF_FF00_0000_0000,
        mmio_end=0xFFFF_FF80_0000_0000,
        max_steps=1,
    )

    assert result.steps_executed == 1
    assert rejected == ["nested execution"]


def test_framebuffer_render_from_execution_callback_fails_before_relocking():
    state = _mp64_accel.CPUState()
    program = bytearray([0x91])  # OUT1; callback runs inside run_steps().
    vram = bytearray([0xE0, 0x07])
    state.attach_mem(program, len(program))
    state.attach_vram(vram, REGION_BASE, len(vram))
    state.fb_base_addr = REGION_BASE
    state.fb_width = 1
    state.fb_height = 1
    state.fb_stride = 2
    state.fb_mode = 1
    state.psel = 3
    state.xsel = 2
    state.set_reg(3, 0)
    state.set_reg(2, 0)
    rejected = []

    def on_output(_port, _value):
        with pytest.raises(
            RuntimeError, match="^CPUState framebuffer render is busy$"
        ):
            state.render_fb_rgb()
        rejected.append("render")

    result = _mp64_accel.run_steps(
        state,
        mmio_read8=lambda _addr: 0,
        mmio_write8=lambda _addr, _value: None,
        on_output=on_output,
        csr_read_override=None,
        mmio_start=0xFFFF_FF00_0000_0000,
        mmio_end=0xFFFF_FF80_0000_0000,
        max_steps=1,
    )

    assert result.steps_executed == 1
    assert rejected == ["render"]


def _call_native_once(state, entry_point):
    kwargs = dict(
        mmio_read8=lambda _addr: 0,
        mmio_write8=lambda _addr, _value: None,
        on_output=lambda _port, _value: None,
        csr_read_override=None,
        mmio_start=0xFFFF_FF00_0000_0000,
        mmio_end=0xFFFF_FF80_0000_0000,
    )
    if entry_point == "step_one":
        return _mp64_accel.step_one(state, **kwargs)
    return _mp64_accel.run_steps(state, max_steps=1, **kwargs)


@pytest.mark.parametrize("entry_point", ("step_one", "run_steps"))
def test_render_first_native_execution_waits_then_progresses(entry_point):
    state = _mp64_accel.CPUState()
    program = bytearray([0x01])  # NOP
    side = 1024
    backing = bytearray(side * side * 4)
    state.attach_mem(program, len(program))
    state.attach_vram(backing, REGION_BASE, len(backing))
    state.fb_base_addr = REGION_BASE
    state.fb_width = side
    state.fb_height = side
    state.fb_stride = side * 4
    state.fb_mode = 3
    state.psel = 3
    state.xsel = 2
    state.set_reg(3, 0)
    state.set_reg(2, 0)

    started = threading.Event()
    stop = threading.Event()
    finished = threading.Event()
    frames = []
    failures = []

    def render_until_observed():
        started.set()
        try:
            while not stop.is_set():
                image = state.render_fb_rgb()
                assert image.shape == (side, side, 3)
                frames.append(image)
        except BaseException as exc:  # propagate worker failures to the test
            failures.append(exc)
        finally:
            finished.set()

    worker = threading.Thread(target=render_until_observed)
    worker.start()
    assert started.wait(timeout=2)

    # A failed no-op setter proves the worker owns the exclusive guard.  The
    # worker cannot destroy that guard until it reacquires the GIL, which this
    # thread retains until the native execution binding releases it to wait.
    deadline = time.monotonic() + 5
    while time.monotonic() < deadline:
        try:
            state.vram_size = len(backing)
        except RuntimeError as exc:
            assert str(exc) == (
                "memory attachments cannot be changed while "
                "CPUState memory is in use"
            )
            break
        time.sleep(0)
    else:
        stop.set()
        worker.join(timeout=5)
        pytest.fail("did not observe an active framebuffer render")

    assert not finished.is_set()
    stop.set()
    result = _call_native_once(state, entry_point)

    worker.join(timeout=5)
    assert not worker.is_alive()
    assert finished.is_set()
    assert failures == []
    assert frames
    assert state.get_reg(3) == 1
    if entry_point == "step_one":
        assert result == 1
    else:
        assert result.steps_executed == 1


def test_framebuffer_render_blocks_mapping_replacement():
    state = _mp64_accel.CPUState()
    side = 2048
    backing = bytearray(side * side * 4)
    state.attach_vram(backing, REGION_BASE, len(backing))
    state.fb_base_addr = REGION_BASE
    state.fb_width = side
    state.fb_height = side
    state.fb_stride = side * 4
    state.fb_mode = 3

    started = threading.Event()
    stop = threading.Event()
    finished = threading.Event()
    failures = []

    def render():
        started.set()
        try:
            while not stop.is_set():
                image = state.render_fb_rgb()
                assert image.shape == (side, side, 3)
        except BaseException as exc:  # propagate worker failures to the test
            failures.append(exc)
        finally:
            finished.set()

    worker = threading.Thread(target=render)
    worker.start()
    assert started.wait(timeout=2)

    deadline = time.monotonic() + 5
    while time.monotonic() < deadline:
        try:
            state.vram_size = len(backing)
        except RuntimeError as exc:
            assert str(exc) == (
                "memory attachments cannot be changed while "
                "CPUState memory is in use"
            )
            break
        time.sleep(0)
    else:
        stop.set()
        worker.join(timeout=5)
        pytest.fail("did not observe an active framebuffer render")

    assert not finished.is_set()
    stop.set()
    with pytest.raises(RuntimeError, match="while CPUState memory is in use"):
        state.attach_vram(bytearray(16), REGION_BASE + 0x100, 16)

    worker.join(timeout=5)
    assert not worker.is_alive()
    assert finished.is_set()
    assert failures == []


def test_rgb565_render_reads_an_odd_host_offset_bytewise():
    state = _mp64_accel.CPUState()
    backing = bytearray([0xCC, 0x00, 0xF8, 0xE0, 0x07])
    state.attach_vram(backing, REGION_BASE, len(backing))
    state.fb_base_addr = REGION_BASE + 1
    state.fb_width = 2
    state.fb_height = 1
    state.fb_stride = 4
    state.fb_mode = 1

    image = state.render_fb_rgb()

    assert image.shape == (2, 1, 3)
    assert image[:, 0, :].tolist() == [[248, 0, 0], [0, 252, 0]]


@pytest.mark.parametrize(
    ("mode", "backing"),
    (
        pytest.param(0, bytearray([0x01]), id="clipped-row"),
        pytest.param(2, bytearray([0x01, 0x02]), id="unknown-mode"),
    ),
)
def test_framebuffer_render_zero_initializes_unwritten_pixels(mode, backing):
    state = _mp64_accel.CPUState()
    state.attach_vram(backing, REGION_BASE, len(backing))
    state.fb_base_addr = REGION_BASE
    state.fb_width = 2
    state.fb_height = 1
    state.fb_stride = 2
    state.fb_mode = mode

    image = state.render_fb_rgb()

    assert image.shape == (2, 1, 3)
    assert image.tolist() == [[[0, 0, 0]], [[0, 0, 0]]]


class _JoiningBufferExporter:
    """PEP 688 exporter that joins a CPU execution from a buffer callback."""

    def __init__(self, storage, callback, callback_phase):
        self.storage = storage
        self.callback = callback
        self.callback_phase = callback_phase
        self.join_timed_out = False
        self.worker_failures = []
        self.workers = []

    def _execute_and_join(self):
        def execute():
            try:
                self.callback()
            except BaseException as exc:
                self.worker_failures.append(exc)

        worker = threading.Thread(target=execute)
        self.workers.append(worker)
        worker.start()
        worker.join(timeout=2)
        self.join_timed_out = self.join_timed_out or worker.is_alive()

    def __buffer__(self, _flags):
        if self.callback_phase == "acquire":
            self._execute_and_join()
        return memoryview(self.storage)

    def __release_buffer__(self, _view):
        if self.callback_phase == "release":
            self._execute_and_join()

    def finish_workers(self):
        for worker in self.workers:
            worker.join(timeout=5)


@pytest.mark.parametrize("region", REGIONS, ids=lambda region: region.name)
def test_buffer_acquisition_callback_can_join_execution_without_lock_cycle(
    region: Region,
):
    state = _mp64_accel.CPUState()
    state.attach_mem(bytearray([0x01, 0x01]), 2)
    state.psel = 3
    state.xsel = 2
    state.set_reg(3, 0)
    state.set_reg(2, 0)
    exporter = _JoiningBufferExporter(
        bytearray([0x01, 0x01]),
        lambda: _call_native_once(state, "step_one"),
        "acquire",
    )

    _attach_native(state, region, exporter, 2)
    exporter.finish_workers()

    assert not exporter.join_timed_out
    assert exporter.worker_failures == []
    assert state.get_reg(3) == 1


@pytest.mark.parametrize("region", REGIONS, ids=lambda region: region.name)
def test_buffer_release_callback_can_join_execution_without_lock_cycle(
    region: Region,
):
    state = _mp64_accel.CPUState()
    state.attach_mem(bytearray([0x01, 0x01]), 2)
    state.psel = 3
    state.xsel = 2
    state.set_reg(3, 0)
    state.set_reg(2, 0)
    exporter = _JoiningBufferExporter(
        bytearray([0x01, 0x01]),
        lambda: _call_native_once(state, "step_one"),
        "release",
    )
    _attach_native(state, region, exporter, 2)

    _attach_native(state, region, bytearray([0x01, 0x01]), 2)
    exporter.finish_workers()

    assert not exporter.join_timed_out
    assert exporter.worker_failures == []
    assert state.get_reg(3) == 1
