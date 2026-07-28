"""Accelerated unbounded TACC external-PHY execution contracts."""

from __future__ import annotations

import pytest

from accel_wrapper import Megapad64 as NativeMegapad64
from asm import assemble
from megapad64 import (
    EXTERNAL_PHY_TIMEOUT_CYCLES,
    EW_U8,
    EW_U16,
    IVEC_BUS_FAULT,
    TACC_IMAGE_BYTES,
    ExternalPhyWordRequest,
    ExternalPhyWordResponse,
    TrapError,
)


CODE_BASE = 0
EXT_BASE = 0x10_0000
EXT_SIZE = 512


def _cpu_with_external(
    *,
    fill: int = 0,
) -> tuple[NativeMegapad64, bytearray]:
    cpu = NativeMegapad64(mem_size=4096, core_id=2)
    external = bytearray([fill]) * EXT_SIZE
    cpu.attach_ext_mem(external, EXT_BASE, len(external))
    cpu.tmode = EW_U8
    cpu.tacc_owner = cpu.core_id
    cpu.tacc_valid = True
    cpu.tacc_dirty = True
    cpu.tacc_format_ew = EW_U8
    cpu.tacc_format_signed = 0
    return cpu, external


def _forbid_python_fallback(cpu: NativeMegapad64) -> None:
    """Turn an accidental MEX fallback into an explicit dispatch failure."""

    def forbidden_fallback() -> int:
        pytest.fail(
            "external TACC instruction designated for native dispatch "
            "entered Python fallback"
        )

    cpu._step_python_fallback = forbidden_fallback


def _step(
    cpu: NativeMegapad64,
    instruction: str,
) -> tuple[bytes, int]:
    code = bytes(assemble(instruction))
    cpu.load_bytes(CODE_BASE, code)
    cpu.pc = CODE_BASE
    return code, cpu.step()


def _tacc_snapshot(cpu: NativeMegapad64) -> tuple[object, ...]:
    return (
        bytes(cpu.tacc),
        int(cpu.tacc_owner),
        bool(cpu.tacc_valid),
        bool(cpu.tacc_dirty),
        int(cpu.tacc_format_ew),
        int(cpu.tacc_format_signed),
        bool(cpu.tacc_busy),
        bool(cpu.tacc_force_pending),
        int(cpu.tacc_epoch),
    )


def test_native_external_load_latency_two_serializes_every_phy_word():
    cpu, external = _cpu_with_external()
    incoming = bytes(range(TACC_IMAGE_BYTES))
    external[:TACC_IMAGE_BYTES] = incoming
    cpu.tacc = bytes([0xA5]) * TACC_IMAGE_BYTES
    cpu.tsrc0 = EXT_BASE
    requests: list[ExternalPhyWordRequest] = []

    def response_plan(
        request: ExternalPhyWordRequest,
    ) -> ExternalPhyWordResponse:
        requests.append(request)
        return ExternalPhyWordResponse(latency_cycles=2)

    cpu.set_external_phy_response_plan(response_plan)
    _forbid_python_fallback(cpu)

    code, cycles = _step(cpu, "t.acc.load")

    assert cycles == 66
    assert cpu.pc == len(code)
    assert bytes(cpu.tacc) == incoming
    assert cpu.tacc_valid
    assert not cpu.tacc_dirty
    assert cpu.cycle_count == 66
    assert cpu.perf_cycles == 66
    assert cpu.perf_stalls == 60
    assert cpu.perf_extmem == 32
    assert cpu.perf_tileops == 1
    assert requests == [
        ExternalPhyWordRequest(
            direction="load",
            beat_index=index // 8,
            word_index=index % 8,
            address=EXT_BASE + index * 8,
            write_data=None,
        )
        for index in range(32)
    ]


def test_native_external_load_error_is_atomic_with_exact_metadata():
    cpu, external = _cpu_with_external()
    incoming = bytes(range(TACC_IMAGE_BYTES))
    external[:TACC_IMAGE_BYTES] = incoming
    cpu.tacc = bytes([0xA5]) * TACC_IMAGE_BYTES
    cpu.tacc_format_ew = EW_U16
    cpu.tacc_format_signed = 1
    cpu.tsrc0 = EXT_BASE
    before = _tacc_snapshot(cpu)
    requests: list[ExternalPhyWordRequest] = []

    def response_plan(
        request: ExternalPhyWordRequest,
    ) -> ExternalPhyWordResponse:
        requests.append(request)
        if request.beat_index == 1 and request.word_index == 3:
            return ExternalPhyWordResponse(
                latency_cycles=4,
                error=True,
            )
        return ExternalPhyWordResponse()

    cpu.set_external_phy_response_plan(response_plan)
    _forbid_python_fallback(cpu)
    code = bytes(assemble("t.acc.load"))
    cpu.load_bytes(CODE_BASE, code)
    cpu.pc = CODE_BASE

    with pytest.raises(TrapError) as raised:
        cpu.step()

    expected_request = ExternalPhyWordRequest(
        direction="load",
        beat_index=1,
        word_index=3,
        address=EXT_BASE + 64 + 3 * 8,
        write_data=None,
    )
    error = raised.value
    assert error.ivec_id == IVEC_BUS_FAULT
    assert error.external_phy_fault == "error"
    assert error.external_phy_request == expected_request
    assert error.fault_cycles == 17
    assert requests[-1] == expected_request
    assert len(requests) == 12
    assert cpu.pc == len(code)
    assert cpu.trap_addr == expected_request.address
    assert _tacc_snapshot(cpu) == before
    assert external[:TACC_IMAGE_BYTES] == incoming
    assert cpu.cycle_count == 17
    assert cpu.perf_cycles == 17
    assert cpu.perf_stalls == 13
    assert cpu.perf_extmem == 11
    assert cpu.perf_tileops == 0


@pytest.mark.parametrize("dirty", (False, True), ids=("clean", "dirty"))
def test_native_external_store_error_preserves_80_byte_prefix_and_dirty(
    dirty: bool,
):
    cpu, external = _cpu_with_external(fill=0xCC)
    image = bytes(range(TACC_IMAGE_BYTES))
    cpu.tacc = image
    cpu.tacc_dirty = dirty
    cpu.tdst = EXT_BASE
    before = _tacc_snapshot(cpu)
    requests: list[ExternalPhyWordRequest] = []

    def response_plan(
        request: ExternalPhyWordRequest,
    ) -> ExternalPhyWordResponse:
        requests.append(request)
        if request.beat_index == 1 and request.word_index == 2:
            return ExternalPhyWordResponse(error=True)
        return ExternalPhyWordResponse()

    cpu.set_external_phy_response_plan(response_plan)
    _forbid_python_fallback(cpu)
    code = bytes(assemble("t.acc.store"))
    cpu.load_bytes(CODE_BASE, code)
    cpu.pc = CODE_BASE

    with pytest.raises(TrapError) as raised:
        cpu.step()

    acknowledged_prefix = 80
    expected_request = ExternalPhyWordRequest(
        direction="store",
        beat_index=1,
        word_index=2,
        address=EXT_BASE + acknowledged_prefix,
        write_data=int.from_bytes(
            image[acknowledged_prefix:acknowledged_prefix + 8],
            "little",
        ),
    )
    error = raised.value
    assert error.ivec_id == IVEC_BUS_FAULT
    assert error.external_phy_fault == "error"
    assert error.external_phy_request == expected_request
    assert error.fault_cycles == 13
    assert requests[-1] == expected_request
    assert len(requests) == 11
    assert cpu.pc == len(code)
    assert cpu.trap_addr == expected_request.address
    assert external[:acknowledged_prefix] == image[:acknowledged_prefix]
    assert external[acknowledged_prefix:] == bytes([0xCC]) * (
        EXT_SIZE - acknowledged_prefix
    )
    assert _tacc_snapshot(cpu) == before
    assert cpu.tacc_dirty is dirty
    assert cpu.cycle_count == 13
    assert cpu.perf_cycles == 13
    assert cpu.perf_stalls == 9
    assert cpu.perf_extmem == 10
    assert cpu.perf_tileops == 0


@pytest.mark.parametrize(
    "terminal_response",
    (
        pytest.param(None, id="no-response"),
        pytest.param(
            ExternalPhyWordResponse(
                latency_cycles=EXTERNAL_PHY_TIMEOUT_CYCLES + 1
            ),
            id="late-response",
        ),
    ),
)
def test_native_external_load_times_out_at_beat_zero_word_two(
    terminal_response: ExternalPhyWordResponse | None,
):
    cpu, external = _cpu_with_external()
    incoming = bytes(range(TACC_IMAGE_BYTES))
    external[:TACC_IMAGE_BYTES] = incoming
    cpu.tacc = bytes([0x5A]) * TACC_IMAGE_BYTES
    cpu.tsrc0 = EXT_BASE
    before = _tacc_snapshot(cpu)
    requests: list[ExternalPhyWordRequest] = []

    def response_plan(
        request: ExternalPhyWordRequest,
    ) -> ExternalPhyWordResponse | None:
        requests.append(request)
        if request.beat_index == 0 and request.word_index == 2:
            return terminal_response
        return ExternalPhyWordResponse()

    cpu.set_external_phy_response_plan(response_plan)
    _forbid_python_fallback(cpu)
    code = bytes(assemble("t.acc.load"))
    cpu.load_bytes(CODE_BASE, code)
    cpu.pc = CODE_BASE

    with pytest.raises(TrapError) as raised:
        cpu.step()

    expected_request = ExternalPhyWordRequest(
        direction="load",
        beat_index=0,
        word_index=2,
        address=EXT_BASE + 2 * 8,
        write_data=None,
    )
    error = raised.value
    assert error.ivec_id == IVEC_BUS_FAULT
    assert error.external_phy_fault == "timeout"
    assert error.external_phy_request == expected_request
    assert error.fault_cycles == 259
    assert requests == [
        ExternalPhyWordRequest(
            direction="load",
            beat_index=0,
            word_index=index,
            address=EXT_BASE + index * 8,
            write_data=None,
        )
        for index in range(3)
    ]
    assert cpu.pc == len(code)
    assert cpu.trap_addr == expected_request.address
    assert _tacc_snapshot(cpu) == before
    assert external[:TACC_IMAGE_BYTES] == incoming
    assert cpu.cycle_count == 259
    assert cpu.perf_cycles == 259
    assert cpu.perf_stalls == 256
    assert cpu.perf_extmem == 2
    assert cpu.perf_tileops == 0


def test_native_external_response_on_255_cycle_deadline_succeeds():
    cpu, external = _cpu_with_external()
    incoming = bytes(range(TACC_IMAGE_BYTES))
    external[:TACC_IMAGE_BYTES] = incoming
    cpu.tsrc0 = EXT_BASE
    requests: list[ExternalPhyWordRequest] = []

    def response_plan(
        request: ExternalPhyWordRequest,
    ) -> ExternalPhyWordResponse:
        requests.append(request)
        if request.beat_index == 0 and request.word_index == 0:
            return ExternalPhyWordResponse(
                latency_cycles=EXTERNAL_PHY_TIMEOUT_CYCLES
            )
        return ExternalPhyWordResponse()

    cpu.set_external_phy_response_plan(response_plan)
    _forbid_python_fallback(cpu)

    code, cycles = _step(cpu, "t.acc.load")

    assert cycles == 288
    assert cpu.pc == len(code)
    assert bytes(cpu.tacc) == incoming
    assert len(requests) == 32
    assert cpu.cycle_count == 288
    assert cpu.perf_cycles == 288
    assert cpu.perf_stalls == 282
    assert cpu.perf_extmem == 32
    assert cpu.perf_tileops == 1


def test_native_external_plan_is_not_consulted_for_internal_image_load():
    cpu, _ = _cpu_with_external()
    source = 0x400
    incoming = bytes(range(TACC_IMAGE_BYTES))
    cpu.mem[source:source + TACC_IMAGE_BYTES] = incoming
    cpu.tsrc0 = source

    def forbidden_plan(_request: ExternalPhyWordRequest):
        pytest.fail("internal TACC image traffic reached the external PHY")

    cpu.set_external_phy_response_plan(forbidden_plan)
    _forbid_python_fallback(cpu)

    code, cycles = _step(cpu, "t.acc.load")

    assert cycles == 6
    assert cpu.pc == len(code)
    assert bytes(cpu.tacc) == incoming
    assert cpu.perf_extmem == 0
    assert cpu.perf_tileops == 1


def test_native_external_plan_is_not_consulted_for_tamac_sources():
    cpu, external = _cpu_with_external()
    external[:64] = bytes([2]) * 64
    external[64:128] = bytes([3]) * 64
    cpu.tacc = bytes(TACC_IMAGE_BYTES)
    cpu.tsrc0 = EXT_BASE
    cpu.tsrc1 = EXT_BASE + 64

    def forbidden_plan(_request: ExternalPhyWordRequest):
        pytest.fail("TAMAC source traffic reached the image-transfer PHY")

    cpu.set_external_phy_response_plan(forbidden_plan)
    _forbid_python_fallback(cpu)

    code, cycles = _step(cpu, "t.amac")

    assert cycles == 7
    assert cpu.pc == len(code)
    assert int.from_bytes(bytes(cpu.tacc[:4]), "little") == 6
    assert cpu.perf_extmem == 0
    assert cpu.perf_tileops == 1
