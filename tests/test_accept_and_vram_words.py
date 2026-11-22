"""ACCEPT, VRAM-BASE and VRAM-SIZE on the emulator's BIOS and in the simulator.

The hosted simulator gives the three BIOS words the BIOS's results. Each
case runs the same Forth on both machines and requires the same report,
the same bytes in the buffer and the same bytes left unread.

ACCEPT ( addr max -- n ) stops at max before it reads a key, ends at a CR
or an LF that it neither stores nor echoes, stores and echoes every other
byte as it came, and ends the line with CR LF. On the emulator the bytes
ACCEPT reads follow the command line on the same UART; in the simulator
they wait in the runtime's UART input while the command is evaluated.
"""

from __future__ import annotations

import os
import re

import pytest

from asm import assemble
from simulator.platform import create_one_core_address_space
from simulator.runtime import BlockedExecution, ExecutionResult, IdleWake
from simulator.runtime import MegaForthRuntime
from system import VRAM_BASE, VRAM_DEFAULT_SIZE, MegapadSystem


BUFFER_BYTES = 16
# The report is framed by EMIT so that the emulator's echo of the command
# line, which holds none of these bytes, cannot be mistaken for it.
_REPORT = re.compile(rb"<(.*?)\|(-?\d+) >", re.S)
_ADDRESS = re.compile(rb"@(\d+) ")


def _accept_command(maximum: int) -> bytes:
    return b"60 EMIT ABUF %d ACCEPT 124 EMIT . 62 EMIT" % maximum


_BIOS_CODE: bytes | None = None


def _bios_code() -> bytes:
    global _BIOS_CODE
    if _BIOS_CODE is None:
        root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        with open(os.path.join(root, "bios.asm"), encoding="utf-8") as source:
            _BIOS_CODE = assemble(source.read())
    return _BIOS_CODE


class _Emulator:
    """The emulator's machine running the BIOS."""

    def __init__(self, *, vram_size: int = VRAM_DEFAULT_SIZE) -> None:
        self.system = MegapadSystem(ram_size=1 << 20, vram_size=vram_size)
        self._sent = bytearray()
        self.system.uart.on_tx = self._sent.append
        self.system.load_binary(0, _bios_code())
        self.system.boot()
        self.run([b"CREATE ABUF %d ALLOT\n" % BUFFER_BYTES])
        output = self.run([b"64 EMIT ABUF . \n"])
        self.buffer = int(_ADDRESS.search(output).group(1))

    def run(self, chunks: list[bytes]) -> bytes:
        """Give the UART each chunk once the BIOS waits for more input."""
        start = len(self._sent)
        position = 0
        steps = 0
        while steps < 50_000_000:
            cpu = self.system.cpu
            if cpu.halted:
                break
            if cpu.idle and not self.system.uart.has_rx_data:
                if position == len(chunks):
                    break
                self.system.uart.inject_input(chunks[position])
                position += 1
                continue
            steps += max(self.system.run_batch(100_000), 1)
        assert position == len(chunks), "the emulator did not take every chunk"
        return bytes(self._sent[start:])

    def accept(self, maximum: int, typed: bytes) -> tuple[bytes, int, bytes]:
        chunks = [_accept_command(maximum) + b"\n"]
        if typed:
            chunks.append(typed)
        report = _REPORT.search(self.run(chunks))
        assert report is not None
        echoed, count = report.group(1), int(report.group(2))
        stored = bytes(
            self.system.cpu.mem[self.buffer:self.buffer + BUFFER_BYTES])
        return echoed, count, stored

    def numbers(self, line: bytes) -> bytes:
        return self.run([b"60 EMIT " + line + b" 62 EMIT\n"])


class _Simulator:
    """The hosted simulator with the same memory map."""

    def __init__(self, *, vram_size: int = VRAM_DEFAULT_SIZE) -> None:
        memory = create_one_core_address_space(
            bank0_size=1 << 20, vram_size=vram_size)
        self.runtime = MegaForthRuntime(memory=memory)
        self.runtime.evaluate(b"CREATE ABUF %d ALLOT" % BUFFER_BYTES)
        self.buffer = self.runtime.find("ABUF").body_address

    def accept(self, maximum: int, typed: bytes) -> tuple[bytes, int, bytes]:
        self.runtime.drain_uart_output()
        self.runtime.inject_uart_input(typed)
        self.runtime.evaluate(_accept_command(maximum))
        report = _REPORT.search(self.runtime.drain_uart_output())
        assert report is not None
        echoed, count = report.group(1), int(report.group(2))
        stored = self.runtime.memory.read_bytes(self.buffer, BUFFER_BYTES)
        return echoed, count, stored

    def numbers(self, line: bytes) -> bytes:
        self.runtime.drain_uart_output()
        self.runtime.evaluate(b"60 EMIT " + line + b" 62 EMIT")
        return self.runtime.drain_uart_output()


@pytest.fixture(scope="module")
def machines():
    return _Emulator(), _Simulator()


# (maximum, typed, echo, count, left unread)
ACCEPT_CASES = (
    pytest.param(10, b"hello\n", b"hello\r\n", 5, b"", id="lf-ends"),
    pytest.param(10, b"hi\r", b"hi\r\n", 2, b"", id="cr-ends"),
    pytest.param(3, b"abc\n", b"abc\r\n", 3, b"\n", id="max-reached"),
    pytest.param(0, b"", b"\r\n", 0, b"", id="max-zero-reads-nothing"),
    pytest.param(10, b"\n", b"\r\n", 0, b"", id="empty-line"),
    pytest.param(
        10, b"ab\x08c\x7f\n", b"ab\x08c\x7f\r\n", 5, b"", id="bytes-unedited"),
)


@pytest.mark.parametrize("maximum, typed, echo, count, unread", ACCEPT_CASES)
def test_accept_reads_a_line_as_the_bios_does(
    machines, maximum, typed, echo, count, unread
) -> None:
    emulator, simulator = machines
    emulator_result = emulator.accept(maximum, typed)
    simulator_result = simulator.accept(maximum, typed)

    echoed, accepted, stored = simulator_result
    assert (echoed, accepted) == (echo, count)
    stripped = typed.rstrip(b"\r\n")
    assert stored[:count] == stripped[:count]
    assert simulator_result == emulator_result
    assert simulator.runtime.uart_input == unread
    # A byte left unread stays for the next reader.  The emulator's BIOS
    # reads it as an empty command line; the simulator's KEY takes it here.
    for _ in unread:
        simulator.runtime.execute("KEY", context=simulator.runtime.new_context())


def test_accept_waits_for_input_and_resumes_as_key_does() -> None:
    simulator = _Simulator()
    runtime = simulator.runtime
    context = runtime.new_context()
    context.data.push(simulator.buffer)
    context.data.push(8)

    blocked = runtime.run_until_blocked("ACCEPT", context=context)
    assert isinstance(blocked, BlockedExecution)
    assert runtime.uart_output == b""

    runtime.inject_uart_input(b"ok")
    wake = runtime.deliver_idle_wake(blocked.suspension, IdleWake.INTERRUPT)
    second = runtime.resume(blocked.suspension, wake)
    assert isinstance(second, BlockedExecution)
    assert runtime.uart_output == b"ok"

    runtime.inject_uart_input(b"\r")
    wake = runtime.deliver_idle_wake(second.suspension, IdleWake.INTERRUPT)
    finished = runtime.resume(second.suspension, wake)
    assert isinstance(finished, ExecutionResult)
    assert context.data.snapshot() == (2,)
    assert runtime.memory.read_bytes(simulator.buffer, 2) == b"ok"
    assert runtime.uart_output == b"ok\r\n"


@pytest.mark.parametrize(
    "vram_size",
    (pytest.param(VRAM_DEFAULT_SIZE, id="4-mib"), pytest.param(0, id="none")),
)
def test_vram_words_report_the_window_system_info_holds(vram_size) -> None:
    expected = (
        b"<%d %d >" % (VRAM_BASE, vram_size) if vram_size else b"<0 0 >")
    emulator = _Emulator(vram_size=vram_size)
    simulator = _Simulator(vram_size=vram_size)
    line = b"VRAM-BASE . VRAM-SIZE ."
    assert expected in emulator.numbers(line)
    assert simulator.numbers(line) == expected
