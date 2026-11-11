"""BOOT-ID in the hosted simulator.

The BIOS hashes its own code and the first file it loads.  This runtime has
no BIOS image: it stands for the BIOS that ``bios.asm`` assembles to, and an
image boot hands it the file the BIOS would have loaded first.
``tests/test_system.py`` (``TestBIOSBootId``) holds the BIOS to the same
cells.
"""

from __future__ import annotations

from shared.boot_identity import bios_code, boot_identity, fold_bytes, take
from simulator.image_bootstrap import prepare_image_bootstrap
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from simulator.storage import HostedStorageService
from test_image_bootstrap import _boot_image, _boot_source


def _asked(runtime: MegaForthRuntime) -> int:
    runtime.execute("BOOT-ID")
    (value,) = runtime.main_context.data.snapshot()
    runtime.main_context.data.pop()
    return value


def test_without_a_boot_file_it_is_the_bioss_own_code() -> None:
    runtime = MegaForthRuntime()

    assert _asked(runtime) == boot_identity()
    assert boot_identity() == fold_bytes(0, bios_code()) != 0


def test_the_boot_file_is_part_of_it_and_it_is_fixed_once_asked() -> None:
    runtime = MegaForthRuntime()
    runtime.boot_file = b": FIRST 1 ;\n"

    first = _asked(runtime)
    assert first == boot_identity(b": FIRST 1 ;\n") != boot_identity()
    runtime.boot_file = b": OTHER 2 ;\n"
    assert _asked(runtime) == first


def test_an_image_boot_gives_the_file_the_bios_would_load_first() -> None:
    prepared = prepare_image_bootstrap(
        memory=create_one_core_address_space(),
        storage=HostedStorageService(_boot_image()),
        terminal_cols=96,
        terminal_rows=32,
    )

    assert prepared.runtime.boot_file == _boot_source()
    assert _asked(prepared.runtime) == boot_identity(_boot_source())


def test_the_hash_takes_cells_then_bytes_and_every_byte_counts() -> None:
    data = bytes(range(1, 20))
    by_hand = 0
    for start in (0, 8):
        by_hand = take(by_hand, int.from_bytes(data[start : start + 8], "little"))
    for byte in data[16:]:
        by_hand = take(by_hand, byte)

    assert fold_bytes(0, data) == by_hand
    seen = {fold_bytes(0, data)}
    for index in range(len(data)):
        changed = bytearray(data)
        changed[index] ^= 0x80
        seen.add(fold_bytes(0, bytes(changed)))
    assert len(seen) == len(data) + 1
