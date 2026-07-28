"""Pure-Python contracts for external TACC image PHY transfers."""

from __future__ import annotations

import pytest

from asm import assemble
from megapad64 import (
    EXTERNAL_PHY_TIMEOUT_CYCLES,
    EW_U8,
    EW_U16,
    IVEC_BUS_FAULT,
    TACC_IMAGE_BYTES,
    ExternalPhyWordRequest,
    ExternalPhyWordResponse,
    Megapad64,
    TrapError,
)


CODE_BASE = 0
EXT_BASE = 0x10_0000
EXT_SIZE = 512


def _cpu_with_external(
    *,
    fill: int = 0,
) -> tuple[Megapad64, bytearray]:
    cpu = Megapad64(mem_size=4096, core_id=2)
    external = bytearray([fill]) * EXT_SIZE
    cpu.attach_ext_mem(external, EXT_BASE, len(external))
    cpu.tmode = EW_U8
    cpu.tacc_owner = cpu.core_id
    cpu.tacc_valid = True
    cpu.tacc_dirty = True
    cpu.tacc_format_ew = EW_U8
    cpu.tacc_format_signed = 0
    return cpu, external


def _step(cpu: Megapad64, instruction: str) -> tuple[bytes, int]:
    code = bytes(assemble(instruction))
    cpu.load_bytes(CODE_BASE, code)
    cpu._icache_invalidate_all(reset_statistics=False)
    cpu.pc = CODE_BASE
    return code, cpu.step()


def _tacc_snapshot(cpu: Megapad64) -> tuple:
    return (
        bytes(cpu.tacc),
        cpu.tacc_owner,
        cpu.tacc_valid,
        cpu.tacc_dirty,
        cpu.tacc_format_ew,
        cpu.tacc_format_signed,
        cpu.tacc_busy,
        cpu.tacc_force_pending,
        cpu.tacc_epoch,
    )


def test_unconfigured_external_load_serializes_and_counts_all_phy_words():
    cpu, external = _cpu_with_external()
    incoming = bytes(range(TACC_IMAGE_BYTES))
    external[:TACC_IMAGE_BYTES] = incoming
    cpu.tacc[:] = bytes([0xA5]) * TACC_IMAGE_BYTES
    cpu.tsrc0 = EXT_BASE

    code, cycles = _step(cpu, "t.acc.load")

    assert cycles == 34
    assert cpu.pc == len(code)
    assert bytes(cpu.tacc) == incoming
    assert cpu.tacc_valid
    assert not cpu.tacc_dirty
    assert cpu.cycle_count == 34
    assert cpu.perf_cycles == 34
    assert cpu.perf_stalls == 28
    assert cpu.perf_extmem == 32
    assert cpu.perf_tileops == 1


def test_unconfigured_external_store_serializes_and_clears_dirty():
    cpu, external = _cpu_with_external(fill=0xCC)
    image = bytes(range(TACC_IMAGE_BYTES))
    cpu.tacc[:] = image
    cpu.tdst = EXT_BASE

    code, cycles = _step(cpu, "t.acc.store")

    assert cycles == 34
    assert cpu.pc == len(code)
    assert external[:TACC_IMAGE_BYTES] == image
    assert external[TACC_IMAGE_BYTES:] == bytes([0xCC]) * (
        EXT_SIZE - TACC_IMAGE_BYTES
    )
    assert cpu.tacc_valid
    assert not cpu.tacc_dirty
    assert cpu.cycle_count == 34
    assert cpu.perf_cycles == 34
    assert cpu.perf_stalls == 28
    assert cpu.perf_extmem == 32
    assert cpu.perf_tileops == 1


def test_configured_latency_observes_all_words_and_becomes_stalls():
    cpu, external = _cpu_with_external()
    incoming = bytes(range(TACC_IMAGE_BYTES))
    external[:TACC_IMAGE_BYTES] = incoming
    cpu.tsrc0 = EXT_BASE
    requests: list[ExternalPhyWordRequest] = []

    def response_plan(
        request: ExternalPhyWordRequest,
    ) -> ExternalPhyWordResponse:
        requests.append(request)
        return ExternalPhyWordResponse(latency_cycles=2)

    cpu.set_external_phy_response_plan(response_plan)

    _, cycles = _step(cpu, "t.acc.load")

    assert cycles == 66
    assert bytes(cpu.tacc) == incoming
    assert cpu.cycle_count == 66
    assert cpu.perf_cycles == 66
    assert cpu.perf_stalls == 60
    assert cpu.perf_extmem == 32
    assert [
        (
            request.direction,
            request.beat_index,
            request.word_index,
            request.address,
            request.write_data,
        )
        for request in requests
    ] == [
        ("load", index // 8, index % 8, EXT_BASE + index * 8, None)
        for index in range(32)
    ]


def test_external_plan_is_not_consulted_for_internal_tacc_load():
    cpu, _ = _cpu_with_external()
    source = 0x400
    incoming = bytes(range(TACC_IMAGE_BYTES))
    cpu.mem[source:source + TACC_IMAGE_BYTES] = incoming
    cpu.tsrc0 = source

    def forbidden_plan(_request: ExternalPhyWordRequest):
        raise AssertionError("internal TACC traffic must not reach the PHY")

    cpu.set_external_phy_response_plan(forbidden_plan)

    _, cycles = _step(cpu, "t.acc.load")

    assert cycles == 6
    assert bytes(cpu.tacc) == incoming
    assert cpu.perf_extmem == 0


def test_external_plan_is_not_consulted_for_tamac_sources():
    cpu, external = _cpu_with_external()
    external[:64] = bytes([2]) * 64
    external[64:128] = bytes([3]) * 64
    cpu.tacc[:] = bytes(TACC_IMAGE_BYTES)
    cpu.tsrc0 = EXT_BASE
    cpu.tsrc1 = EXT_BASE + 64

    def forbidden_plan(_request: ExternalPhyWordRequest):
        raise AssertionError("TAMAC sources do not use the image-transfer PHY")

    cpu.set_external_phy_response_plan(forbidden_plan)

    _, cycles = _step(cpu, "t.amac")

    assert cycles == 7
    assert int.from_bytes(cpu.tacc[:4], "little") == 6
    assert cpu.perf_extmem == 0


def test_explicit_load_error_keeps_tacc_and_partial_beat_private():
    cpu, external = _cpu_with_external()
    incoming = bytes(range(TACC_IMAGE_BYTES))
    external[:TACC_IMAGE_BYTES] = incoming
    cpu.tacc[:] = bytes([0xA5]) * TACC_IMAGE_BYTES
    cpu.tacc_format_ew = EW_U16
    cpu.tacc_format_signed = 1
    cpu.tsrc0 = EXT_BASE
    before = _tacc_snapshot(cpu)
    acquired = []
    updates = []
    released = []

    def acquire(direction, base, ew, signed, image):
        acquired.append((direction, base, ew, signed, image))
        return "stage"

    def update(token, beat_index, image):
        updates.append((token, beat_index, image))
        return True

    def release(token):
        released.append(token)
        return True

    cpu._tacc_image_stage_acquire_hook = acquire
    cpu._tacc_image_stage_update_hook = update
    cpu._tacc_image_stage_release_hook = release

    def response_plan(
        request: ExternalPhyWordRequest,
    ) -> ExternalPhyWordResponse:
        if request.beat_index == 1 and request.word_index == 3:
            return ExternalPhyWordResponse(
                latency_cycles=4,
                error=True,
            )
        return ExternalPhyWordResponse()

    cpu.set_external_phy_response_plan(response_plan)
    code = bytes(assemble("t.acc.load"))
    cpu.load_bytes(CODE_BASE, code)
    cpu.pc = CODE_BASE

    with pytest.raises(TrapError) as raised:
        cpu.step()

    error = raised.value
    assert error.ivec_id == IVEC_BUS_FAULT
    assert error.external_phy_fault == "error"
    assert error.external_phy_request == ExternalPhyWordRequest(
        direction="load",
        beat_index=1,
        word_index=3,
        address=EXT_BASE + 64 + 3 * 8,
        write_data=None,
    )
    assert cpu.pc == len(code)
    assert cpu.trap_addr == EXT_BASE + 64 + 3 * 8
    assert _tacc_snapshot(cpu) == before
    assert acquired == [
        ("load", EXT_BASE, EW_U8, False, bytes(TACC_IMAGE_BYTES))
    ]
    assert updates == [
        (
            "stage",
            1,
            incoming[:64] + bytes(TACC_IMAGE_BYTES - 64),
        )
    ]
    assert released == ["stage"]
    assert cpu.cycle_count == 17
    assert cpu.perf_cycles == 17
    assert cpu.perf_stalls == 13
    assert cpu.perf_extmem == 11
    assert cpu.perf_tileops == 0


@pytest.mark.parametrize(
    "terminal_response",
    (
        None,
        ExternalPhyWordResponse(
            latency_cycles=EXTERNAL_PHY_TIMEOUT_CYCLES + 1
        ),
    ),
)
def test_no_response_times_out_at_255_cycles_with_exact_word_address(
    terminal_response,
):
    cpu, external = _cpu_with_external()
    external[:TACC_IMAGE_BYTES] = bytes(range(TACC_IMAGE_BYTES))
    cpu.tacc[:] = bytes([0x5A]) * TACC_IMAGE_BYTES
    cpu.tsrc0 = EXT_BASE
    before = _tacc_snapshot(cpu)

    def response_plan(request: ExternalPhyWordRequest):
        if request.beat_index == 0 and request.word_index == 2:
            return terminal_response
        return ExternalPhyWordResponse()

    cpu.set_external_phy_response_plan(response_plan)

    with pytest.raises(TrapError) as raised:
        _step(cpu, "t.acc.load")

    error = raised.value
    assert error.ivec_id == IVEC_BUS_FAULT
    assert error.external_phy_fault == "timeout"
    assert error.external_phy_request.address == EXT_BASE + 2 * 8
    assert cpu.trap_addr == EXT_BASE + 2 * 8
    assert _tacc_snapshot(cpu) == before
    assert cpu.cycle_count == 259
    assert cpu.perf_cycles == 259
    assert cpu.perf_stalls == 256
    assert cpu.perf_extmem == 2
    assert cpu.perf_tileops == 0


def test_response_on_255_cycle_deadline_wins_over_timeout():
    cpu, external = _cpu_with_external()
    incoming = bytes(range(TACC_IMAGE_BYTES))
    external[:TACC_IMAGE_BYTES] = incoming
    cpu.tsrc0 = EXT_BASE

    def response_plan(
        request: ExternalPhyWordRequest,
    ) -> ExternalPhyWordResponse:
        if request.beat_index == 0 and request.word_index == 0:
            return ExternalPhyWordResponse(
                latency_cycles=EXTERNAL_PHY_TIMEOUT_CYCLES
            )
        return ExternalPhyWordResponse()

    cpu.set_external_phy_response_plan(response_plan)

    _, cycles = _step(cpu, "t.acc.load")

    assert cycles == 288
    assert bytes(cpu.tacc) == incoming
    assert cpu.perf_stalls == 282
    assert cpu.perf_extmem == 32


@pytest.mark.parametrize("dirty", (False, True))
def test_store_error_preserves_acknowledged_word_prefix_and_dirty(dirty):
    cpu, external = _cpu_with_external(fill=0xCC)
    image = bytes(range(TACC_IMAGE_BYTES))
    cpu.tacc[:] = image
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

    with pytest.raises(TrapError) as raised:
        _step(cpu, "t.acc.store")

    acknowledged_prefix = (8 + 2) * 8
    error = raised.value
    assert error.ivec_id == IVEC_BUS_FAULT
    assert error.external_phy_fault == "error"
    assert cpu.trap_addr == EXT_BASE + acknowledged_prefix
    assert external[:acknowledged_prefix] == image[:acknowledged_prefix]
    assert external[acknowledged_prefix:] == bytes([0xCC]) * (
        EXT_SIZE - acknowledged_prefix
    )
    assert _tacc_snapshot(cpu) == before
    assert len(requests) == 11
    assert requests[-1] == ExternalPhyWordRequest(
        direction="store",
        beat_index=1,
        word_index=2,
        address=EXT_BASE + acknowledged_prefix,
        write_data=int.from_bytes(
            image[acknowledged_prefix:acknowledged_prefix + 8],
            "little",
        ),
    )
    assert cpu.cycle_count == 13
    assert cpu.perf_cycles == 13
    assert cpu.perf_stalls == 9
    assert cpu.perf_extmem == 10
    assert cpu.perf_tileops == 0
