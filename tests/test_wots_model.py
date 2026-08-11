"""Focused contract tests for the Python WOTS controller/reference model."""

from __future__ import annotations

import hashlib

import pytest

from devices import (
    BusError,
    DeviceBus,
    WOTS_BASE,
    WOTS_DMA_RESPONSE_MEMORY_TIMEOUT,
    WOTS_DMA_RESPONSE_OK,
    WOTS_DMA_RESPONSE_PROTOCOL,
    WOTS_DMA_RESPONSE_TARGET_FAULT,
    WOTS_ERROR_ACCEPT_TIMEOUT,
    WOTS_ERROR_CONTEXT_SPAN,
    WOTS_ERROR_INTERNAL,
    WOTS_ERROR_INVALID_COMMAND,
    WOTS_ERROR_MEMORY_TIMEOUT,
    WOTS_ERROR_OWNER,
    WOTS_ERROR_START,
    WOTS_ERROR_STEPS,
    WOTS_ERROR_TARGET_FAULT,
    WOTS_STATUS_BUSY,
    WOTS_STATUS_DONE,
    WOTS_STATUS_ERROR,
    WOTS_STATUS_IDLE,
    WotsChainAccel,
)


class FakeSharedKeccak:
    """One externally owned permutation service; never a WOTS sub-core."""

    def __init__(self, *, available: bool = True, complete: bool = True):
        self.available = available
        self.complete = complete
        self.claimed = False
        self.quiescent = True
        self.claim_count = 0
        self.release_count = 0
        self.abort_count = 0
        self.submitted: list[bytes] = []
        self._results: list[bytes] = []

    def claim_wots(self) -> bool:
        if not self.available or self.claimed:
            return False
        self.claimed = True
        self.quiescent = True
        self.claim_count += 1
        return True

    def submit_wots_state(self, state: bytes) -> bool:
        if not self.claimed or len(state) != 200:
            return False
        state = bytes(state)
        self.submitted.append(state)
        self.quiescent = False
        if self.complete:
            result = bytearray(200)
            result[:16] = hashlib.shake_256(state[:64]).digest(16)
            self._results.append(bytes(result))
            self.quiescent = True
        return True

    def take_wots_result(self):
        if not self._results:
            return None
        return self._results.pop(0)

    def abort_wots(self) -> None:
        self.abort_count += 1
        self._results.clear()

    def wots_quiescent(self) -> bool:
        return self.quiescent

    def release_wots(self) -> None:
        self.claimed = False
        self.quiescent = True
        self.release_count += 1


def _write_request(
    device: WotsChainAccel,
    context_addr: int,
    start: int,
    steps: int,
) -> None:
    for index in range(8):
        device.write8(index, (context_addr >> (8 * index)) & 0xFF)
    device.write8(0x08, steps)
    device.write8(0x09, start)


def _read_u32(device: WotsChainAccel, offset: int) -> int:
    return sum(device.read8(offset + index) << (8 * index) for index in range(4))


def _feed_context(device: WotsChainAccel, context: bytes) -> list[int]:
    addresses: list[int] = []
    for value in context:
        active, beat = device.cycle_dma_view()
        assert active
        assert beat is not None
        addresses.append(beat.address)
        assert device.cycle_dma_accept(beat.token)
        device.tick(1)
        assert device.cycle_dma_complete(
            beat.token,
            response_code=WOTS_DMA_RESPONSE_OK,
            read_value=value,
        )
    return addresses


def _independent_chain_reference(
    context: bytes,
    start: int,
    steps: int,
) -> bytes:
    """Evaluate the selected WOTS construction without controller code."""
    seed = context[:16]
    adrs = context[16:48]
    node = context[48:64]
    for index in range(steps):
        step_adrs = bytearray(adrs)
        step_adrs[28:32] = (start + index).to_bytes(4, "big")
        node = hashlib.shake_256(seed + step_adrs + node).digest(16)
    return node


def test_wots_byte_only_access_map_preflights_before_mutation() -> None:
    device = WotsChainAccel(bank0_size=0x1000)
    bus = DeviceBus()
    bus.register(device)

    bus.write8(WOTS_BASE, 0xA5)
    assert bus.read8(WOTS_BASE) == 0xA5

    for width in (2, 4, 8):
        with pytest.raises(BusError):
            bus.preflight_access(WOTS_BASE, width)
        with pytest.raises(BusError):
            bus.preflight_access(WOTS_BASE, width, write=True)
    for offset in range(0x0B, 0x20):
        with pytest.raises(BusError):
            bus.write8(WOTS_BASE + offset, 0xFF)

    assert bus.read8(WOTS_BASE) == 0xA5
    assert device.read8(0x0B) == 0
    assert bytes(device.read8(0x10 + i) for i in range(16)) == bytes(16)


def test_wots_go_validation_is_ordered_and_preserves_raw_bytes() -> None:
    device = WotsChainAccel(bank0_size=0x100)

    _write_request(device, 0xFF, 0xFF, 0xFF)
    device.write8(0x0A, 1)
    assert device.read8(0x0A) == WOTS_STATUS_ERROR
    assert device.read8(0x0B) == WOTS_ERROR_STEPS
    assert device.read8(0x08) == 0xFF
    assert device.read8(0x09) == 0xFF

    device.write8(0x0A, 2)
    _write_request(device, 0, 16, 0)
    device.write8(0x0A, 1)
    assert device.read8(0x0B) == WOTS_ERROR_START

    device.write8(0x0A, 2)
    _write_request(device, 0xC1, 15, 0)
    device.write8(0x0A, 1)
    assert device.read8(0x0B) == WOTS_ERROR_CONTEXT_SPAN

    device.write8(0x0A, 2)
    _write_request(device, 0, 14, 1)
    device.write8(0x0A, 1)
    assert device.read8(0x0B) == WOTS_ERROR_OWNER


def test_wots_invalid_command_and_terminal_state_are_stable_until_clear() -> None:
    device = WotsChainAccel()
    device.write8(0x0A, 0x81)
    assert device.read8(0x0A) == WOTS_STATUS_ERROR
    assert device.read8(0x0B) == WOTS_ERROR_INVALID_COMMAND

    device.write8(0x00, 0x55)
    device.write8(0x0A, 1)
    device.write8(0x0A, 0x7F)
    assert device.read8(0x00) == 0
    assert device.read8(0x0B) == WOTS_ERROR_INVALID_COMMAND

    device.write8(0x0A, 2)
    assert device.read8(0x0A) == WOTS_STATUS_IDLE
    assert device.read8(0x0B) == 0


def test_wots_zero_step_reads_exact_context_and_never_claims_keccak() -> None:
    service = FakeSharedKeccak(available=False)
    device = WotsChainAccel(
        bank0_size=0x400,
        num_bus_ports=4,
        keccak_service=service,
    )
    context = bytes(range(64))
    _write_request(device, 0x180, 15, 0)
    device.write8(0x0A, 1)

    addresses = _feed_context(device, context)
    assert addresses == list(range(0x180, 0x1C0))
    assert device.read8(0x0A) == WOTS_STATUS_DONE
    assert bytes(device.read8(0x10 + i) for i in range(16)) == context[48:64]
    assert service.claim_count == 0
    assert service.release_count == 0
    assert _read_u32(device, 0x0C) == 64

    retained_cycles = _read_u32(device, 0x0C)
    device.write8(0x00, 0xEE)
    assert device.read8(0x00) == 0x80
    device.write8(0x0A, 2)
    assert device.read8(0x0A) == WOTS_STATUS_IDLE
    assert _read_u32(device, 0x0C) == retained_cycles
    assert bytes(device.read8(0x10 + i) for i in range(16)) == bytes(16)


def test_wots_nonzero_chain_uses_external_shared_service_and_scrubs() -> None:
    service = FakeSharedKeccak()
    device = WotsChainAccel(
        bank0_size=0x1000,
        keccak_service=service,
    )
    seed = bytes(range(16))
    adrs = bytes(range(32, 64))
    node = bytes(range(128, 144))
    context = seed + adrs + node
    _write_request(device, 0x200, 3, 2)
    device.write8(0x0A, 1)
    assert service.claim_count == 1
    _feed_context(device, context)

    device.tick(2)
    assert device.read8(0x0A) == WOTS_STATUS_DONE
    assert service.release_count == 1
    assert not service.claimed
    assert len(service.submitted) == 2

    expected_node = node
    for index, state in enumerate(service.submitted):
        step = 3 + index
        assert state[0:16] == seed
        assert state[16:44] == adrs[:28]
        assert state[44:48] == step.to_bytes(4, "big")
        assert state[48:64] == expected_node
        assert state[64] == 0x1F
        assert state[65:135] == bytes(70)
        assert state[135] == 0x80
        assert state[136:] == bytes(64)
        expected_node = hashlib.shake_256(state[:64]).digest(16)

    assert bytes(device.read8(0x10 + i) for i in range(16)) == expected_node
    assert bytes(device._context) == bytes(64)
    assert bytes(device._keccak_state) == bytes(200)
    assert bytes(device._node) == bytes(16)


def test_wots_every_valid_geometry_matches_independent_reference_at_bank_edges(
) -> None:
    """Qualify all 136 valid pairs at both exact Bank 0 boundary spans."""
    bank0_size = 0x100
    context = bytes((index * 29 + 7) & 0xFF for index in range(64))
    valid_pairs = [
        (start, steps)
        for start in range(16)
        for steps in range(16 - start)
    ]
    assert len(valid_pairs) == 136

    for context_addr in (0, bank0_size - len(context)):
        for start, steps in valid_pairs:
            service = FakeSharedKeccak()
            device = WotsChainAccel(
                bank0_size=bank0_size,
                keccak_service=service,
            )
            _write_request(device, context_addr, start, steps)
            device.write8(0x0A, 1)

            addresses = _feed_context(device, context)
            device.tick(steps)

            assert addresses == list(
                range(context_addr, context_addr + len(context))
            ), (context_addr, start, steps)
            assert device.read8(0x0A) == WOTS_STATUS_DONE, (
                context_addr,
                start,
                steps,
            )
            assert bytes(
                device.read8(0x10 + index) for index in range(16)
            ) == _independent_chain_reference(context, start, steps), (
                context_addr,
                start,
                steps,
            )
            assert service.claim_count == int(steps != 0)
            assert service.release_count == int(steps != 0)
            assert device.private_zeroized()


@pytest.mark.parametrize(
    ("response_code", "expected_error"),
    [
        (WOTS_DMA_RESPONSE_TARGET_FAULT, WOTS_ERROR_TARGET_FAULT),
        (WOTS_DMA_RESPONSE_MEMORY_TIMEOUT, WOTS_ERROR_MEMORY_TIMEOUT),
        (WOTS_DMA_RESPONSE_PROTOCOL, WOTS_ERROR_INTERNAL),
        (17, WOTS_ERROR_INTERNAL),
    ],
)
def test_wots_classifies_terminal_dma_responses(
    response_code: int,
    expected_error: int,
) -> None:
    device = WotsChainAccel(bank0_size=0x100)
    _write_request(device, 0, 0, 0)
    device.write8(0x0A, 1)
    _, beat = device.cycle_dma_view()
    assert beat is not None
    assert device.cycle_dma_accept(beat.token)
    assert device.cycle_dma_complete(
        beat.token,
        response_code=response_code,
        read_value=0xA5,
    )
    assert device.read8(0x0A) == WOTS_STATUS_ERROR
    assert device.read8(0x0B) == expected_error


def test_wots_clear_withdraws_preaccept_and_drains_accepted_beat() -> None:
    device = WotsChainAccel(bank0_size=0x100, num_bus_ports=1)
    _write_request(device, 0, 0, 0)
    device.write8(0x0A, 1)
    _, first = device.cycle_dma_view()
    assert first is not None
    device.write8(0x0A, 2)
    assert device.read8(0x0A) == WOTS_STATUS_IDLE
    assert device.cycle_dma_view() == (False, None)

    _write_request(device, 0, 0, 0)
    device.write8(0x0A, 1)
    _, accepted = device.cycle_dma_view()
    assert accepted is not None
    assert device.cycle_dma_accept(accepted.token)
    device.write8(0x0A, 2)
    assert device.read8(0x0A) == WOTS_STATUS_BUSY
    assert not device.cycle_dma_accept(accepted.token)
    assert device.cycle_dma_complete(
        accepted.token,
        response_code=WOTS_DMA_RESPONSE_OK,
        read_value=0x11,
    )
    assert device.read8(0x0A) == WOTS_STATUS_IDLE
    assert device.cycle_dma_view() == (False, None)


def test_wots_acceptance_on_deadline_edge_wins_and_timeout_is_local() -> None:
    device = WotsChainAccel(bank0_size=0x100, num_bus_ports=1)
    _write_request(device, 0, 0, 0)
    device.write8(0x0A, 1)
    _, beat = device.cycle_dma_view()
    assert beat is not None
    assert device.dma_accept_cycles == 1
    assert device.cycle_dma_accept(beat.token)
    device.tick(1)
    assert device.read8(0x0A) == WOTS_STATUS_BUSY

    device.reset()
    _write_request(device, 0, 0, 0)
    device.write8(0x0A, 1)
    device.tick(1)
    assert device.read8(0x0A) == WOTS_STATUS_ERROR
    assert device.read8(0x0B) == WOTS_ERROR_ACCEPT_TIMEOUT


def test_wots_clear_waits_for_shared_keccak_quiescence() -> None:
    service = FakeSharedKeccak(complete=False)
    device = WotsChainAccel(bank0_size=0x100, keccak_service=service)
    _write_request(device, 0, 0, 1)
    device.write8(0x0A, 1)
    _feed_context(device, bytes(range(64)))
    assert device.read8(0x0A) == WOTS_STATUS_BUSY

    device.write8(0x0A, 2)
    assert service.abort_count == 1
    service.quiescent = False
    device.tick(7)
    assert device.read8(0x0A) == WOTS_STATUS_BUSY
    device.write8(0x0A, 2)
    assert device.read8(0x0A) == WOTS_STATUS_BUSY

    service.quiescent = True
    device.tick(1)
    assert device.read8(0x0A) == WOTS_STATUS_IDLE
    assert service.release_count == 1


def test_wots_cycles_saturate_and_reset_only_on_go_or_machine_reset() -> None:
    device = WotsChainAccel(bank0_size=0x100)
    _write_request(device, 0, 0, 0)
    device.write8(0x0A, 1)
    _, beat = device.cycle_dma_view()
    assert beat is not None
    assert device.cycle_dma_accept(beat.token)
    device._cycles = 0xFFFF_FFFE
    device.tick(10)
    assert _read_u32(device, 0x0C) == 0xFFFF_FFFF
    device.write8(0x0A, 2)
    assert device.cycle_dma_complete(
        beat.token,
        response_code=WOTS_DMA_RESPONSE_OK,
        read_value=0,
    )
    assert _read_u32(device, 0x0C) == 0xFFFF_FFFF
    device.reset()
    assert _read_u32(device, 0x0C) == 0
