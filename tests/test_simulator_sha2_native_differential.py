"""Focused hosted/native SHA-2 terminal-semantic differential vectors.

This module intentionally lives outside ``tests/simulator`` because its
oracle requires the compiled MP64 backend.  It executes only the native
per-core SHA-2 instructions against a private ``CPUState`` memory mapping;
there is no BIOS, KDOS, scheduler, or system boot in this qualification seam.
"""

from __future__ import annotations

import hashlib

import _mp64_accel
import pytest

from asm import assemble
from simulator.memory import MMIO_BASE, MMIO_LIMIT, SparseAddressSpace
from simulator.sha2 import SHA2_STATUS_OK, HostedSHA2Service


NATIVE_MEMORY_BYTES = 0x4000
NATIVE_SCRATCH = 0x2000
HOSTED_SOURCE = 0x0100
HOSTED_DESTINATION = 0x1000


def _step(state) -> int:
    return _mp64_accel.step_one(
        state,
        mmio_read8=lambda _address: 0,
        mmio_write8=lambda _address, _value: None,
        on_output=lambda _port, _value: None,
        csr_read_override=None,
        mmio_start=MMIO_BASE,
        mmio_end=MMIO_LIMIT,
    )


def _set_pc(state, address: int) -> None:
    state.psel = 3
    state.xsel = 2
    state.set_reg(3, address)


def _packed_native_words(state, algorithm: str) -> tuple[int, ...]:
    if algorithm == "sha256":
        words = []
        for index in range(4):
            packed = state.get_acc(index)
            words.extend((packed >> 32, packed & 0xFFFF_FFFF))
        return tuple(words)
    return tuple(
        [state.get_acc(index) for index in range(4)]
        + [state.get_reg(index) for index in range(16, 20)]
    )


def _native_digest(
    algorithm: str,
    mode: int,
    message: bytes,
) -> tuple[bytes, tuple[int, ...], tuple[int, ...]]:
    source_register = 5
    input_offset_register = 4
    output_register = 6
    output_index_register = 7
    program = assemble(
        "\n".join(
            [f"sha.init {mode}"]
            + [
                f"sha.din r{input_offset_register}, r{source_register}"
                for _ in message
            ]
            + ["sha.final"]
            + [
                f"sha.dout r{output_register}, r{output_index_register}"
                for _ in range(8)
            ]
            + ["halt"]
        )
    )
    memory = bytearray(NATIVE_MEMORY_BYTES)
    memory[: len(program)] = program
    state = _mp64_accel.CPUState()
    state.attach_mem(memory, len(memory))
    state.tsrc0 = NATIVE_SCRATCH
    _set_pc(state, 0)

    _step(state)  # SHA.INIT
    for value in message:
        state.set_reg(source_register, value)
        _step(state)  # SHA.DIN
    _step(state)  # SHA.FINAL

    packed_words = _packed_native_words(state, algorithm)
    output_words = []
    for index in range(8):
        state.set_reg(output_index_register, index)
        _step(state)  # SHA.DOUT
        output_words.append(state.get_reg(output_register))

    word_bytes = 4 if algorithm == "sha256" else 8
    digest = b"".join(
        word.to_bytes(word_bytes, "big") for word in output_words
    )
    return digest, packed_words, tuple(output_words)


def _hosted_digest(algorithm: str, message: bytes) -> bytes:
    memory = SparseAddressSpace(bank0_size=0x2000)
    memory.write_bytes(HOSTED_SOURCE, message)
    service = HostedSHA2Service(core_count=1)
    initialize = getattr(service, f"{algorithm}_init")
    update = getattr(service, f"{algorithm}_update")
    finalize = getattr(service, f"{algorithm}_final")

    assert initialize(0) == SHA2_STATUS_OK
    assert update(0, HOSTED_SOURCE, len(message), memory) == SHA2_STATUS_OK
    assert finalize(0, HOSTED_DESTINATION, memory) == SHA2_STATUS_OK
    return memory.read_bytes(
        HOSTED_DESTINATION,
        hashlib.new(algorithm).digest_size,
    )


@pytest.mark.parametrize(
    ("algorithm", "mode", "length"),
    (
        # SHA-256's 8-byte length field makes 55/56 the one-/two-pad-block
        # transition.  63/64/65 also cross the automatic compression edge.
        ("sha256", 0, 0),
        ("sha256", 0, 55),
        ("sha256", 0, 56),
        ("sha256", 0, 63),
        ("sha256", 0, 64),
        ("sha256", 0, 65),
        # SHA-512 has the corresponding 16-byte length-field transition at
        # 111/112 and its automatic compression edge at 127/128/129.
        ("sha512", 2, 0),
        ("sha512", 2, 111),
        ("sha512", 2, 112),
        ("sha512", 2, 127),
        ("sha512", 2, 128),
        ("sha512", 2, 129),
    ),
)
def test_hosted_sha2_matches_native_at_padding_and_block_boundaries(
    algorithm: str,
    mode: int,
    length: int,
) -> None:
    message = bytes(
        (index * 37 + length * 11 + 0x5A) & 0xFF
        for index in range(length)
    )
    native, packed_words, output_words = _native_digest(
        algorithm,
        mode,
        message,
    )
    hosted = _hosted_digest(algorithm, message)
    expected = hashlib.new(algorithm, message).digest()

    assert packed_words == output_words
    assert native == hosted == expected
