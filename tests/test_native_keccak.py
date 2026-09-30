"""Shared native Keccak values and hosted service dispatch qualification.

Both extensions must be built before this gate. MMIO ownership, framing and
publication remain in HostedSHA3Service; only its pure permutation is replaced.
"""

from __future__ import annotations

from collections.abc import Sequence
import hashlib
import importlib
import random

import pytest

from shared.cells import MASK64
from shared.crypto_caps import CRYPTO_CAP_KECCAK_F1600, CRYPTO_CAP_SHA3_STREAM
from shared.keccak import KECCAK_LANES, keccak_f1600
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from simulator.sha3 import (
    SHA3_COMMAND,
    SHA3_CONTROL,
    SHA3_DATA_INPUT,
    SHA3_DATA_OUTPUT,
    SHA3_ERROR,
    SHA3_STATE_DATA,
    SHA3_STATE_INDEX,
    SHA3_STATUS,
    HostedSHA3Service,
)


CAPABILITIES = CRYPTO_CAP_SHA3_STREAM | CRYPTO_CAP_KECCAK_F1600


@pytest.fixture(params=("_mp64_accel", "_megaforth_native"))
def native_keccak(request):
    return importlib.import_module(request.param).keccak_f1600


def _write(service, offset, value, width=1):
    service.preflight(offset, width, write=True)
    for index in range(width):
        service.write8(offset + index, (value >> (index * 8)) & 0xFF)


def _read(service, offset, width=1):
    service.preflight(offset, width, write=False)
    return sum(service.read8(offset + index) << (index * 8)
               for index in range(width))


def _raw_permutation(service, lanes):
    _write(service, SHA3_COMMAND, 7)
    for index, lane in enumerate(lanes):
        _write(service, SHA3_STATE_INDEX, index)
        _write(service, SHA3_STATE_DATA, lane, 8)
    _write(service, SHA3_COMMAND, 6)
    assert _read(service, SHA3_STATUS) == 0x0A
    assert _read(service, SHA3_ERROR) == 0
    result = []
    for index in range(KECCAK_LANES):
        _write(service, SHA3_STATE_INDEX, index)
        result.append(_read(service, SHA3_STATE_DATA, 8))
    return tuple(result)


def test_zero_state_matches_published_prefix_and_python_oracle(native_keccak):
    lanes = [0] * KECCAK_LANES
    result = native_keccak(lanes)

    assert type(result) is tuple
    assert len(result) == KECCAK_LANES
    assert result[:5] == (
        0xF1258F7940E1DDE7, 0x84D5CCF933C0478A, 0xD598261EA65AA9EE,
        0xBD1547306F80494D, 0x8B284E056253D057,
    )
    assert result == keccak_f1600(lanes)
    assert all(type(lane) is int and 0 <= lane <= MASK64 for lane in result)
    assert lanes == [0] * KECCAK_LANES


def test_seeded_and_boundary_states_match_without_mutating_inputs(native_keccak):
    generator = random.Random(0x4B454343414B)
    states = [
        [MASK64] * KECCAK_LANES,
        list(range(KECCAK_LANES)),
        [1 << ((index * 13) % 64) for index in range(KECCAK_LANES)],
    ]
    states.extend(
        [generator.getrandbits(64) for _ in range(KECCAK_LANES)]
        for _ in range(32)
    )
    for lanes in states:
        before = tuple(lanes)
        expected = keccak_f1600(before)
        result = native_keccak(lanes)
        assert result == native_keccak(before) == expected
        assert type(result) is tuple
        assert tuple(lanes) == before
        lanes[0] ^= MASK64
        assert result == expected


@pytest.mark.parametrize("sequence_kind", ["list", "sequence"])
def test_custom_sequence_iteration_order_matches_python_oracle(native_keccak, sequence_kind):
    class ReversedList(list):
        def __iter__(self):
            return reversed(self)

    class ReversedSequence(Sequence):
        def __init__(self, values):
            self.values = tuple(values)

        def __len__(self):
            return len(self.values)

        def __getitem__(self, index):
            return self.values[index]

        def __iter__(self):
            return reversed(self.values)

    sequence_type = ReversedList if sequence_kind == "list" else ReversedSequence
    lanes = sequence_type(range(KECCAK_LANES))
    expected = keccak_f1600(lanes)

    assert native_keccak(lanes) == expected
    assert expected == keccak_f1600(tuple(reversed(range(KECCAK_LANES))))
    assert [lanes[index] for index in range(KECCAK_LANES)] == list(range(KECCAK_LANES))


@pytest.mark.parametrize("actual_length", [24, 26])
def test_inconsistent_sequence_iteration_length_is_rejected(native_keccak, actual_length):
    class MisreportedLength(list):
        def __len__(self):
            return KECCAK_LANES

    lanes = MisreportedLength(range(actual_length))
    with pytest.raises(ValueError, match="exactly 25"):
        native_keccak(lanes)
    assert list(lanes) == list(range(actual_length))


@pytest.mark.parametrize("lanes", [[], [0] * 24, [0] * 26])
def test_incorrect_lane_count_is_rejected(native_keccak, lanes):
    before = lanes.copy()
    with pytest.raises(ValueError, match="exactly 25"):
        native_keccak(lanes)
    assert lanes == before


@pytest.mark.parametrize("value", [True, False, 1.0, "1", None])
def test_non_integer_lanes_are_rejected_without_mutation(native_keccak, value):
    lanes = [0] * KECCAK_LANES
    lanes[12] = value
    before = lanes.copy()
    with pytest.raises(TypeError, match="uint64"):
        native_keccak(lanes)
    assert lanes == before


@pytest.mark.parametrize("value", [-1, 1 << 64])
def test_out_of_range_lanes_are_rejected_without_wrapping(native_keccak, value):
    lanes = [0] * KECCAK_LANES
    lanes[-1] = value
    before = lanes.copy()
    with pytest.raises(ValueError, match="uint64"):
        native_keccak(lanes)
    assert lanes == before


@pytest.mark.parametrize("value", [None, 0, {0}, {0: 0}])
def test_non_sequence_states_are_rejected(native_keccak, value):
    with pytest.raises(TypeError, match="sequence"):
        native_keccak(value)


def test_generator_is_not_consumed_as_a_lane_sequence(native_keccak):
    lanes = (index for index in range(KECCAK_LANES))
    with pytest.raises(TypeError, match="sequence"):
        native_keccak(lanes)
    assert tuple(lanes) == tuple(range(KECCAK_LANES))


def test_hosted_raw_mmio_uses_native_permutation_and_keeps_binding_on_clear(native_keccak):
    calls = []

    def observe(lanes):
        calls.append(tuple(lanes))
        return native_keccak(lanes)

    service = HostedSHA3Service(CAPABILITIES)
    assert service.bind_native_permutation(observe) is True
    for lanes in ([0] * KECCAK_LANES, list(range(KECCAK_LANES))):
        assert _raw_permutation(service, lanes) == keccak_f1600(lanes)
    assert calls == [tuple([0] * KECCAK_LANES), tuple(range(KECCAK_LANES))]


@pytest.mark.parametrize("mode,rate,algorithm", [
    (0, 136, "sha3_256"), (1, 72, "sha3_512"),
    (2, 168, "shake_128"), (3, 136, "shake_256"),
])
def test_hosted_streaming_absorb_final_and_squeeze_use_native_kernel(
    native_keccak, mode, rate, algorithm,
):
    calls = []

    def observe(lanes):
        calls.append(tuple(lanes))
        return native_keccak(lanes)

    service = HostedSHA3Service(CAPABILITIES)
    assert service.bind_native_permutation(observe) is True
    message = bytes((index * 37 + 11) & 0xFF for index in range(rate + 1))
    _write(service, SHA3_CONTROL, mode)
    _write(service, SHA3_COMMAND, 1)
    for value in message:
        _write(service, SHA3_DATA_INPUT, value)
    assert len(calls) == 1
    _write(service, SHA3_COMMAND, 3)
    assert len(calls) == 2

    windows = 1 if mode < 2 else 3
    output = bytearray()
    for index in range(windows):
        if index:
            _write(service, SHA3_COMMAND, 4)
        output.extend(_read(service, SHA3_DATA_OUTPUT + offset)
                      for offset in range(64))
        assert _read(service, SHA3_STATUS) == 0x06
        assert _read(service, SHA3_ERROR) == 0
    digest = getattr(hashlib, algorithm)(message)
    expected = digest.digest().ljust(64, b"\0") if mode < 2 else digest.digest(192)
    assert bytes(output) == expected
    assert len(calls) == (2 if mode < 2 else 3)


def test_explicit_host_permutation_survives_native_binding_attempt(native_keccak):
    calls = []

    def custom_permutation(lanes):
        calls.append(tuple(lanes))
        return keccak_f1600(lanes)

    service = HostedSHA3Service(CAPABILITIES, permutation=custom_permutation)
    assert service.bind_native_permutation(native_keccak) is False
    lanes = tuple(range(KECCAK_LANES))
    assert _raw_permutation(service, lanes) == keccak_f1600(lanes)
    service.bind_native_permutation(None)
    assert _raw_permutation(service, lanes) == keccak_f1600(lanes)
    assert calls == [lanes, lanes]


def test_custom_service_subclass_preserves_its_operation(native_keccak):
    calls = []

    class CustomService(HostedSHA3Service):
        def _complete_raw(self):
            calls.append(tuple(self._state))
            super()._complete_raw()

    service = CustomService(CAPABILITIES)
    assert service.bind_native_permutation(native_keccak) is False
    lanes = tuple(range(KECCAK_LANES))
    assert _raw_permutation(service, lanes) == keccak_f1600(lanes)
    assert calls == [lanes]


def test_existing_python_oracle_override_survives_native_binding(native_keccak, monkeypatch):
    sha3_module = importlib.import_module("simulator.sha3")
    calls = []

    def custom_permutation(lanes):
        calls.append(tuple(lanes))
        return keccak_f1600(lanes)

    monkeypatch.setattr(sha3_module, "keccak_f1600", custom_permutation)
    service = HostedSHA3Service(CAPABILITIES)
    assert service.bind_native_permutation(native_keccak) is False
    lanes = tuple(range(KECCAK_LANES))

    assert _raw_permutation(service, lanes) == keccak_f1600(lanes)
    assert calls == [lanes]


def test_runtime_native_selection_and_python_reuse_choose_current_policy(monkeypatch):
    extension = importlib.import_module("_megaforth_native")
    original = extension.keccak_f1600
    native_calls = []

    def observe(lanes):
        native_calls.append(tuple(lanes))
        return original(lanes)

    monkeypatch.setattr(extension, "keccak_f1600", observe)
    memory = create_one_core_address_space()
    native = MegaForthRuntime(memory=memory, execution_backend="native")
    lanes = tuple(range(KECCAK_LANES))
    assert _raw_permutation(native.sha3, lanes) == keccak_f1600(lanes)
    assert native_calls == [lanes]

    reference = MegaForthRuntime(memory=memory, execution_backend="python")
    assert reference.sha3 is native.sha3
    assert _raw_permutation(reference.sha3, lanes) == keccak_f1600(lanes)
    assert native_calls == [lanes]


def test_runtime_native_selection_preserves_injected_permutation(monkeypatch):
    extension = importlib.import_module("_megaforth_native")
    native_calls = []
    custom_calls = []

    def reject_native(lanes):
        native_calls.append(tuple(lanes))
        raise AssertionError("native selection replaced the injected permutation")

    def custom_permutation(lanes):
        custom_calls.append(tuple(lanes))
        return keccak_f1600(lanes)

    monkeypatch.setattr(extension, "keccak_f1600", reject_native)
    memory = create_one_core_address_space()
    memory.mmio.sha3 = HostedSHA3Service(CAPABILITIES, permutation=custom_permutation)
    lanes = tuple(range(KECCAK_LANES))
    for backend in ("native", "python"):
        runtime = MegaForthRuntime(memory=memory, execution_backend=backend)
        assert _raw_permutation(runtime.sha3, lanes) == keccak_f1600(lanes)
    assert custom_calls == [lanes, lanes]
    assert native_calls == []
