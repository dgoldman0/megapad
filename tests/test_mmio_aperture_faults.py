"""An access past the devices' 4 GiB of the MMIO aperture is a bus fault.

Every device answers at an MMIO offset below 2^32, the span the RTL's bus
decodes as MMIO. The emulator's aperture runs on to 2^39 bytes, and an
access at a higher offset faults as one outside every device window does,
on a native core and on a core that Python runs. It reaches no device: the
offset's low 32 bits used to name the device a native core then answered
from, and a core that Python runs stopped with a host TypeError.
"""

from __future__ import annotations

import pytest

from asm import assemble
from devices import MBOX_BASE, SYSINFO_BASE, UART_BASE
from megapad64 import IVEC_BUS_FAULT, TrapError
from system import MMIO_END, MMIO_START, MegapadSystem


HIGH = 1 << 32
# The low 32 bits name the UART, System Info, the mailbox's SEND register
# and the chiplet window; the last address is the aperture's final word.
# A write stores 1, which the UART would send and SEND would take as core
# 1's ID.
ADDRESSES = [
    MMIO_START + HIGH + UART_BASE,
    MMIO_START + HIGH + SYSINFO_BASE,
    MMIO_START + HIGH + MBOX_BASE + 0x08,
    MMIO_START + HIGH + 0xE00,
    MMIO_END - 8,
]


def _system() -> MegapadSystem:
    system = MegapadSystem(
        ram_size=1 << 16, num_cores=2, num_clusters=0,
        hbw_size=0, ext_mem_size=0, vram_size=0,
    )
    sent = bytearray()
    system.uart.on_tx = sent.append
    system.sent = sent
    return system


@pytest.mark.parametrize("address", ADDRESSES, ids=hex)
@pytest.mark.parametrize("width", [1, 2, 4, 8])
def test_a_core_that_python_runs_faults(address, width):
    system = _system()
    cpu = system.cpu
    for access in (
        lambda: getattr(cpu, f"mem_read{8 * width}")(address),
        lambda: getattr(cpu, f"mem_write{8 * width}")(address, 1),
    ):
        cpu.trap_addr = 0
        with pytest.raises(TrapError) as fault:
            access()
        assert fault.value.ivec_id == IVEC_BUS_FAULT
        assert cpu.trap_addr == address
    assert not system.sent
    assert system.mailbox.pending == [0, 0]


@pytest.mark.parametrize("address", ADDRESSES, ids=hex)
@pytest.mark.parametrize(
    "operation", ["ld.b r1, r2", "st.b r2, r1", "ld.h r1, r2", "st.w r2, r1",
                  "ldn r1, r2", "str r2, r1"],
)
def test_a_native_core_faults(address, operation):
    system = _system()
    cpu = system.cpu
    cpu.load_bytes(0, assemble(
        f"ldi r1, 1\nldi64 r2, {address}\n{operation}\nhalt\n"))
    cpu.pc = 0
    cpu.step()
    cpu.step()
    with pytest.raises(TrapError) as fault:
        cpu.step()
    assert fault.value.ivec_id == IVEC_BUS_FAULT
    assert cpu.trap_addr == address
    assert not system.sent
    assert system.mailbox.pending == [0, 0]
