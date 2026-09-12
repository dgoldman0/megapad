"""Native scalar access spans pages while retaining the shared byte oracle."""
from __future__ import annotations

import pytest

pytest.importorskip("_megaforth_native")

from simulator.memory import AddressClass
from tests.simulator.test_native_execution import _compare, _runtimes


@pytest.mark.parametrize("page_size", [1, 4, 8, 16, 4096])
@pytest.mark.parametrize("materialized", [False, True])
def test_scalar_widths_cross_pages_with_and_without_existing_destinations(page_size, materialized):
    runtimes = _runtimes(external_size=8192, page_size=page_size)
    for runtime in runtimes:
        region = next(r for r in runtime.memory.regions if r.kind is AddressClass.EXTERNAL)
        target = region.base + 4093
        if materialized:
            runtime.memory.write_bytes(target, bytes(15))
        runtime.define_constant("TARGET", target)
        runtime.evaluate(
            b": RUN 0x8877665544332211 TARGET ! "
            b"0xAABBCCDD TARGET 8 + L! 0xEEFF TARGET 12 + W! "
            b"0x123 TARGET 14 + C! TARGET @ TARGET 8 + L@ "
            b"TARGET 12 + W@ TARGET 14 + C@ ;"
        )
    observed = _compare(runtimes, "RUN", spans=((target, 15),))
    assert observed["error"] is None
    assert observed["data"] == (0x8877665544332211, 0xAABBCCDD, 0xEEFF, 0x23)
    assert observed["memory"] == (bytes.fromhex("1122334455667788ddccbbaaffee23"),)


@pytest.mark.parametrize("page_size", [1, 4, 8, 4096])
def test_sparse_read_fragments_and_fallback_materialization_remain_visible(page_size):
    runtimes = _runtimes(external_size=8192, page_size=page_size)
    for runtime in runtimes:
        region = next(r for r in runtime.memory.regions if r.kind is AddressClass.EXTERNAL)
        target = region.base + 4093
        runtime.memory.write8(target + 4, 0xAB)
        runtime.define_constant("TARGET", target)
        runtime.evaluate(b": RUN TARGET @ 0xCD TARGET C! TARGET @ ;")
    observed = _compare(runtimes, "RUN", spans=((target, 8),))
    assert observed["error"] is None
    assert observed["data"] == (0xAB00000000, 0xAB000000CD)
    assert observed["memory"] == (bytes.fromhex("cd000000ab000000"),)
