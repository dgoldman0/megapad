"""Address/fetch fusion preserves intermediate stack writes and fault order."""
import pytest

pytest.importorskip("_megaforth_native")

from shared.cells import MASK64
from simulator.ir import Branch, Call, Literal, Return
from simulator.memory import AddressClass
from simulator.stacks import DataStack, StackOverflow
from tests.simulator.test_native_execution import _compare, _runtimes


FETCHES = (b"@", b"C@", b"W@", b"L@")


@pytest.mark.parametrize("fetch", FETCHES)
@pytest.mark.parametrize("kind", ("literal", "constant", "created"))
@pytest.mark.parametrize("budget", range(1, 7))
def test_fetch_fusion_keeps_every_original_budget_effect(fetch, kind, budget):
    runtimes = _runtimes(b"CREATE ITEM 16 ALLOT")
    for runtime in runtimes:
        address = runtime.find("ITEM").body_address
        runtime.memory.write64(address, 0x8877665544332211)
        if kind == "literal":
            runtime.define_colon("RUN", (Literal(address), Call(runtime.find(fetch).xt), Return()))
        else:
            name = b"ITEM"
            if kind == "constant":
                runtime.define_constant("ADDRESS", address)
                name = b"ADDRESS"
            runtime.evaluate(b": RUN " + name + b" " + fetch + b" ;")
    _compare(runtimes, "RUN", step_budget=budget, require_native=False)


@pytest.mark.parametrize("fetch", FETCHES)
@pytest.mark.parametrize("offset", (-7, -4, -1, 0, 1, 4, 7, 8))
def test_fetch_observes_the_address_push_when_its_read_aliases_that_slot(fetch, offset):
    runtimes = _runtimes(page_size=16)
    for runtime in runtimes:
        slot = runtime.main_context.data.empty_pointer - 16
        runtime.memory.write_bytes(slot - 16, bytes(range(32)))
        runtime.define_constant("ADDRESS", slot + offset)
        runtime.evaluate(b": RUN ADDRESS " + fetch + b" ;")
    observed = _compare(runtimes, "RUN", inputs=(0x1122334455667788,))
    assert observed["error"] is None


@pytest.mark.parametrize("fetch", FETCHES)
@pytest.mark.parametrize("target", ("sparse", "crossing", "wrapped", "return"))
def test_fetch_fusion_keeps_sparse_reads_and_reference_fault_paths(fetch, target):
    runtimes = _runtimes(external_size=4096, page_size=16)
    for runtime in runtimes:
        region = next(r for r in runtime.memory.regions if r.kind is AddressClass.EXTERNAL)
        address = {
            "sparse": region.base + 15,
            "crossing": region.base + region.size - 1,
            "wrapped": MASK64,
            "return": runtime.main_context.returns.empty_pointer - 8,
        }[target]
        runtime.define_constant("ADDRESS", address)
        runtime.evaluate(b": RUN ADDRESS " + fetch + b" ;")
    _compare(runtimes, "RUN", require_native=False)


def test_fetch_fusion_cannot_omit_the_address_push_overflow():
    runtimes = _runtimes(b"CREATE ITEM 8 ALLOT : RUN ITEM @ ;")
    for runtime in runtimes:
        context = runtime.main_context
        empty = context.data.empty_pointer
        context.data = DataStack(memory=runtime.memory, floor=empty - 8, empty_pointer=empty)
    observed = _compare(runtimes, "RUN", inputs=(17,), require_native=False)
    assert observed["error"][0] is StackOverflow
    assert observed["data"] == (17,)


@pytest.mark.parametrize("fetch", FETCHES)
def test_original_fetch_entry_remains_available_to_branches(fetch):
    runtimes = _runtimes(b"CREATE ITEM 8 ALLOT")
    for runtime in runtimes:
        address = runtime.find("ITEM").body_address
        runtime.memory.write64(address, 0x1234)
        runtime.define_colon("RUN", (Branch(2), Literal(99), Call(runtime.find(fetch).xt), Return()))
    observed = _compare(runtimes, "RUN", inputs=(address,))
    assert observed["error"] is None
