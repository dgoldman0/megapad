"""Qualified SHA3 byte submission preserves the raw device and its fault seam."""

from __future__ import annotations

from contextlib import contextmanager
import hashlib
import sys

import pytest

from shared.keccak import keccak_f1600
from simulator import memory as memory_module, platform as platform_module, sha3
from simulator.memory import MMIOAccessError, MMIO_BASE, SparseAddressSpace
from simulator.platform import OneCorePlatformMMIO, create_one_core_address_space
from simulator.sha3 import (
    CALLER_SPAN_OK, CRYPTO_STATUS_HARDWARE, CRYPTO_STATUS_OK,
    CRYPTO_STATUS_PROTECTED, CRYPTO_STATUS_STATE, HostedSHA3Service,
    SHA3_COMMAND, SHA3_DATA_INPUT,
)


IDENTITY = (0, 17)
SOURCE = 0x20000
DESTINATION = 0x22000
RATES = (136, 72, 168, 136)
ALGORITHMS = (hashlib.sha3_256, hashlib.sha3_512, hashlib.shake_128, hashlib.shake_256)


def _new(mode=0, *, service=None, memory_type=SparseAddressSpace):
    memory = create_one_core_address_space()
    if memory_type is not SparseAddressSpace:
        memory = memory_type(mmio=memory._mmio)
    if service is not None:
        memory._mmio.sha3 = service
    service = memory._mmio.sha3
    assert service.begin(IDENTITY, mode, memory) == CRYPTO_STATUS_OK
    return memory, service


def _update(memory, service, payload, *, span_status=lambda address, length: CALLER_SPAN_OK):
    memory.write_bytes(SOURCE, payload)
    return service.update(IDENTITY, SOURCE, len(payload), memory=memory, span_status=span_status)


@contextmanager
def _observe_input_routes():
    """Count actual calls without replacing any method used for qualification."""

    counts = {"memory": 0, "platform": 0, "service": 0}
    codes = {
        SparseAddressSpace.write8.__code__: ("memory", "address", MMIO_BASE + SHA3_DATA_INPUT),
        OneCorePlatformMMIO.write8.__code__: ("platform", "offset", SHA3_DATA_INPUT),
        HostedSHA3Service.write8.__code__: ("service", "offset", SHA3_DATA_INPUT),
    }
    previous = sys.getprofile()

    def observe(frame, event, arg):
        if event == "call" and frame.f_code in codes:
            key, parameter, value = codes[frame.f_code]
            if frame.f_locals.get(parameter) == value:
                counts[key] += 1
        if previous is not None:
            previous(frame, event, arg)

    sys.setprofile(observe)
    try:
        yield counts
    finally:
        sys.setprofile(previous)


def _finish(memory, service, mode):
    if mode < 2:
        assert service.final(IDENTITY, DESTINATION, memory=memory,
                             span_status=lambda address, length: CALLER_SPAN_OK) == CRYPTO_STATUS_OK
        return memory.read_bytes(DESTINATION, 32 if mode == 0 else 64)
    assert service.shake_final(IDENTITY, memory) == CRYPTO_STATUS_OK
    output = bytearray()
    # Cross the device's 64-byte output window in several checked reads.
    for size in (19, 32, 17, 32):
        assert service.shake_read(IDENTITY, DESTINATION, size, memory=memory,
                                 span_status=lambda address, length: CALLER_SPAN_OK) == CRYPTO_STATUS_OK
        output.extend(memory.read_bytes(DESTINATION, size))
    assert service.clear(IDENTITY, memory) == CRYPTO_STATUS_OK
    return bytes(output)


@pytest.mark.parametrize("mode", range(4))
@pytest.mark.parametrize("delta", (-1, 0, 1))
def test_qualified_input_keeps_each_raw_byte_at_rate_boundaries(mode, delta):
    memory, service = _new(mode)
    payload = bytes((index * 37 + 11) & 255 for index in range(RATES[mode] + delta))
    assert service._input_transfer_eligible(memory, payload)
    with _observe_input_routes() as calls:
        # Preserve incremental buffering when an earlier update leaves one byte.
        assert _update(memory, service, payload[:1]) == CRYPTO_STATUS_OK
        assert _update(memory, service, payload[1:]) == CRYPTO_STATUS_OK
    assert calls == {"memory": 0, "platform": 0, "service": len(payload)}
    expected = ALGORITHMS[mode](payload)
    assert _finish(memory, service, mode) == (expected.digest() if mode < 2 else expected.digest(100))
    assert service.private_zeroized()


@pytest.mark.parametrize("method", ("write8", "_write_integer", "_mmio_write", "_mmio_preflight"))
@pytest.mark.parametrize("late", (False, True))
def test_memory_overrides_before_or_after_first_transfer_keep_byte_dispatch(monkeypatch, method, late):
    memory, service = _new()
    if late:
        assert _update(memory, service, b"prefix") == CRYPTO_STATUS_OK
    original = getattr(memory, method)
    seen = []

    def observe(*args, **kwargs):
        seen.append(args)
        return original(*args, **kwargs)

    monkeypatch.setattr(memory, method, observe)
    payload = b"custom route"
    assert not service._input_transfer_eligible(memory, payload)
    with _observe_input_routes() as calls:
        assert _update(memory, service, payload) == CRYPTO_STATUS_OK
    assert seen
    # The original memory method is still reached through the custom wrapper.
    assert calls["memory"] == calls["platform"] == calls["service"] == len(payload)


@pytest.mark.parametrize("method", ("preflight", "write8", "_service"))
@pytest.mark.parametrize("before_construction", (False, True))
def test_platform_class_overrides_are_never_blessed_by_first_use(monkeypatch, method, before_construction):
    if not before_construction:
        memory, service = _new()
        assert _update(memory, service, b"prefix") == CRYPTO_STATUS_OK
    original = getattr(OneCorePlatformMMIO, method)
    seen = []

    def observe(self, *args, **kwargs):
        seen.append(args)
        return original(self, *args, **kwargs)

    monkeypatch.setattr(OneCorePlatformMMIO, method, observe)
    if before_construction:
        memory, service = _new()
    seen.clear()
    assert not service._input_transfer_eligible(memory, b"route")
    assert _update(memory, service, b"route") == CRYPTO_STATUS_OK
    assert seen


@pytest.mark.parametrize("method", ("preflight", "write8", "_write_input", "_absorb_buffer", "_rate", "_permute"))
def test_late_service_helpers_keep_the_original_memory_route(monkeypatch, method):
    memory, service = _new()
    assert _update(memory, service, b"prefix") == CRYPTO_STATUS_OK
    original = getattr(service, method)
    seen = []

    def observe(*args, **kwargs):
        seen.append(args)
        return original(*args, **kwargs)

    monkeypatch.setattr(service, method, observe)
    payload = bytes(256)
    assert not service._input_transfer_eligible(memory, payload)
    with _observe_input_routes() as calls:
        assert _update(memory, service, payload) == CRYPTO_STATUS_OK
    assert seen
    assert calls["memory"] == len(payload)


@pytest.mark.parametrize("route", ("memory", "platform", "service", "different_service"))
def test_subclasses_and_a_different_device_owner_decline_qualification(route):
    class CustomMemory(SparseAddressSpace):
        pass

    class CustomPlatform(OneCorePlatformMMIO):
        pass

    class CustomService(HostedSHA3Service):
        pass

    custom = CustomService(sha3.CRYPTO_CAP_SHA3_STREAM) if route == "service" else None
    memory, service = _new(service=custom, memory_type=CustomMemory if route == "memory" else SparseAddressSpace)
    if route == "platform":
        old = memory._mmio
        memory._mmio = CustomPlatform(**{name: getattr(old, name) for name in old.__slots__})
    elif route == "different_service":
        memory._mmio.sha3 = HostedSHA3Service(service.capabilities)
    assert not service._input_transfer_eligible(memory, b"data")


@pytest.mark.parametrize("target,name", (
    (memory_module, "_checked_span"), (memory_module, "_require_integer"),
    (platform_module, "SHA3_OFFSET"), (sha3, "SHA3_DATA_INPUT"),
    (sha3, "_INPUT_PLATFORM_ROUTE"),
))
def test_changed_route_helpers_constants_or_missing_registry_decline(monkeypatch, target, name):
    memory, service = _new()
    original = getattr(target, name)
    replacement = (lambda *args, **kwargs: original(*args, **kwargs)) if callable(original) else None
    monkeypatch.setattr(target, name, replacement)
    assert not service._input_transfer_eligible(memory, b"data")


@pytest.mark.parametrize("binding", ("injected", "native_callback", "module_override"))
def test_custom_permutation_can_replace_memory_route_mid_submission(monkeypatch, binding):
    memory, service = _new()
    original_write = memory.write8
    observed = []

    def observe_write(address, value):
        if address == MMIO_BASE + SHA3_DATA_INPUT:
            observed.append(value)
        return original_write(address, value)

    def permutation(lanes):
        monkeypatch.setattr(memory, "write8", observe_write)
        return keccak_f1600(lanes)

    if binding == "injected":
        service = HostedSHA3Service(service.capabilities, permutation=permutation)
        memory._mmio.sha3 = service
        assert service.begin(IDENTITY, 0, memory) == CRYPTO_STATUS_OK
    elif binding == "native_callback":
        assert service.bind_native_permutation(permutation)
    else:
        monkeypatch.setattr(sha3, "keccak_f1600", permutation)
    payload = bytes(range(139))
    assert not service._input_transfer_eligible(memory, payload)
    assert _update(memory, service, payload) == CRYPTO_STATUS_OK
    assert observed == list(payload[136:])
    assert _finish(memory, service, 0) == hashlib.sha3_256(payload).digest()


@pytest.mark.parametrize("seam", ("preflight", "write"))
def test_custom_faults_preserve_exception_cause_address_and_applied_prefix(monkeypatch, seam):
    memory, service = _new()
    applied = 0
    failure = ValueError("injected input fault")
    original = service.preflight if seam == "preflight" else service.write8

    def fail(offset, *args, **kwargs):
        nonlocal applied
        if offset == SHA3_DATA_INPUT:
            if applied == 3:
                raise failure
            applied += 1
        return original(offset, *args, **kwargs)

    monkeypatch.setattr(service, "preflight" if seam == "preflight" else "write8", fail)
    with pytest.raises(MMIOAccessError) as caught:
        _update(memory, service, b"abcdef")
    error = caught.value
    assert error.__cause__ is failure
    assert (error.operation, error.address, error.length) == ("write", MMIO_BASE + SHA3_DATA_INPUT, 1)
    assert ("rejected write preflight" if seam == "preflight" else "failed during write") in str(error)
    assert service._buffer_length == 3
    assert bytes(service._buffer[:3]) == b"abc"
    assert service.checked_owner == IDENTITY


def test_qualified_failure_has_the_same_partial_device_state_and_wrapper(monkeypatch):
    fast_memory, fast_service = _new()
    slow_memory, slow_service = _new()
    original_write = slow_memory.write8
    monkeypatch.setattr(slow_memory, "write8", lambda address, value: original_write(address, value))
    # A malformed private state makes the original canonical absorb raise
    # after XORing its first lane. Both routes must expose that exact prefix.
    fast_service._state = [0]
    slow_service._state = [0]
    payload = bytes(range(136))
    assert fast_service._input_transfer_eligible(fast_memory, payload)
    errors = []
    for memory, service in ((fast_memory, fast_service), (slow_memory, slow_service)):
        with pytest.raises(MMIOAccessError) as caught:
            _update(memory, service, payload)
        errors.append(caught.value)
    assert str(errors[0]) == str(errors[1])
    assert type(errors[0].__cause__) is type(errors[1].__cause__) is IndexError
    assert fast_service._state == slow_service._state
    assert fast_service._buffer == slow_service._buffer
    assert fast_service._buffer_length == slow_service._buffer_length == 136
    assert fast_service.checked_owner == slow_service.checked_owner == IDENTITY


def test_payload_read_finishes_before_route_is_qualified(monkeypatch):
    memory, service = _new()
    original_read = memory.read_bytes
    original_write = memory.write8
    seen = []

    def replaced_write(address, value):
        if address == MMIO_BASE + SHA3_DATA_INPUT:
            seen.append(value)
        return original_write(address, value)

    def read_and_replace(address, length):
        payload = original_read(address, length)
        monkeypatch.setattr(memory, "write8", replaced_write)
        return payload

    monkeypatch.setattr(memory, "read_bytes", read_and_replace)
    assert _update(memory, service, b"changed on read") == CRYPTO_STATUS_OK
    assert bytes(seen) == b"changed on read"


def test_rejected_span_and_zero_length_have_no_input_effect():
    memory, service = _new()
    with _observe_input_routes() as calls:
        assert service.update(IDENTITY, -1, 0, memory=memory,
                              span_status=lambda *args: pytest.fail("zero length checked a span")) == CRYPTO_STATUS_OK
        assert _update(memory, service, b"protected",
                       span_status=lambda *args: sha3.CALLER_SPAN_PROTECTED) == CRYPTO_STATUS_PROTECTED
    assert calls == {"memory": 0, "platform": 0, "service": 0}
    assert service.private_zeroized()


@pytest.mark.parametrize("length,status", ((136, CRYPTO_STATUS_HARDWARE), (137, CRYPTO_STATUS_STATE)))
def test_operation_failure_and_raw_interference_keep_cleanup_and_publication(length, status):
    memory, service = _new()
    memory.write_bytes(DESTINATION, b"unchanged")
    service.inject_operation_failure_once()
    # A byte after the failed absorb still reaches the same raw device and
    # records its conflict, exactly as the ordinary per-byte loop does.
    assert _update(memory, service, bytes(length)) == status
    assert service.private_zeroized()
    assert memory.read_bytes(DESTINATION, 9) == b"unchanged"

    assert service.begin(IDENTITY, 0, memory) == CRYPTO_STATUS_OK
    # A raw CLEAR is visible through the very same checked service object.
    memory.write8(MMIO_BASE + SHA3_COMMAND, 7)
    assert _update(memory, service, b"after interference") == CRYPTO_STATUS_STATE
    assert service.private_zeroized()


@pytest.mark.parametrize("module_name", ("_megaforth_native", "_mp64_accel"))
def test_canonical_native_permutation_keeps_the_qualified_input_route(module_name):
    native = pytest.importorskip(module_name)
    memory, service = _new()
    assert service.bind_native_permutation(native.keccak_f1600)
    payload = bytes(range(256))
    assert service._input_transfer_eligible(memory, payload)
    with _observe_input_routes() as calls:
        assert _update(memory, service, payload) == CRYPTO_STATUS_OK
    assert calls == {"memory": 0, "platform": 0, "service": len(payload)}
    assert _finish(memory, service, 0) == hashlib.sha3_256(payload).digest()


@pytest.mark.parametrize("replacement", ((), object()))
def test_replaced_platform_registry_declines_without_using_it(monkeypatch, replacement):
    memory, service = _new()
    monkeypatch.setattr(sha3, "_INPUT_PLATFORM_ROUTE", replacement)
    assert not service._input_transfer_eligible(memory, b"data")
    assert _update(memory, service, b"data") == CRYPTO_STATUS_OK


def test_repeated_platform_registration_fails_closed(monkeypatch):
    memory, service = _new()
    monkeypatch.setattr(sha3, "_INPUT_PLATFORM_ROUTE", sha3._INPUT_PLATFORM_ROUTE)
    sha3._register_input_platform_route(OneCorePlatformMMIO)
    assert not service._input_transfer_eligible(memory, b"data")
    assert _update(memory, service, b"data") == CRYPTO_STATUS_OK


@pytest.mark.parametrize("payload_type", (bytearray, type("DerivedBytes", (bytes,), {})))
def test_custom_payload_container_retains_ordinary_submission(monkeypatch, payload_type):
    memory, service = _new()
    original_read = memory.read_bytes
    monkeypatch.setattr(memory, "read_bytes",
                        lambda address, length: payload_type(original_read(address, length)))
    payload = b"payload from a custom reader"
    assert not service._input_transfer_eligible(memory, payload_type(payload))
    with _observe_input_routes() as calls:
        assert _update(memory, service, payload) == CRYPTO_STATUS_OK
    assert calls["memory"] == len(payload)


def test_final_clear_failure_keeps_owner_and_destination_after_qualified_update():
    memory, service = _new()
    memory.write_bytes(DESTINATION, bytes([0xA5]) * 32)
    assert _update(memory, service, b"qualified input") == CRYPTO_STATUS_OK
    service.inject_clear_failure_once()
    assert service.final(IDENTITY, DESTINATION, memory=memory,
                         span_status=lambda address, length: CALLER_SPAN_OK) == CRYPTO_STATUS_HARDWARE
    assert service.checked_owner == IDENTITY
    assert memory.read_bytes(DESTINATION, 32) == bytes([0xA5]) * 32
    assert service.clear(IDENTITY, memory) == CRYPTO_STATUS_OK
    assert service.private_zeroized()
