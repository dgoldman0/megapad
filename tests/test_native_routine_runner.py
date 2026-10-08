"""The hybrid routine runner keeps machine frames on the shared return stack."""

from __future__ import annotations

import threading

import _mp64_accel as native
import pytest

from asm import assemble


MASK64 = (1 << 64) - 1
RAM_SIZE = 0x10000
FLOOR, TOP = 0x8000, 0x10000
EXT_BASE, EXT_SIZE = 0x100000, 0x10000
CODE = EXT_BASE + 0x1000
DATA = EXT_BASE + 0x8000

ADD = """
start:
    add r4, r5
    ret.l
"""
CALLBACK = """
start:
    ldi64 r12, stub
call:
    call.l r12
after:
    add r4, r5
    ret.l
stub:
    ret.l
"""
COUNT = """
start:
loop:
    subi r4, 1
    brne loop
    ret.l
"""
STORE = """
start:
    str r4, r5
    ret.l
"""


class Machine:
    def __init__(self):
        self.ram = bytearray(RAM_SIZE)
        self.ext = bytearray(EXT_SIZE)
        self.state = native.CPUState()
        self.state.attach_mem(self.ram, RAM_SIZE)
        self.state.attach_ext_mem(self.ext, EXT_BASE, EXT_SIZE)
        self.state.icache_control_write(1)
        self.runner = native.RoutineRunner(self.state)
        self.next_code = CODE

    def image(self, source, *, inputs=0, outputs=0, entry="start", sites=(), publish=True):
        base = self.next_code
        labels = {}
        raw = bytes(assemble(source, base_addr=base, labels_out=labels))
        code = raw + b"\x01" * (-len(raw) % 16)
        self.ext[base - EXT_BASE:base - EXT_BASE + len(code)] = code
        self.next_code = base + len(code) + 0x100
        rows = tuple((labels[call] - base, labels[stub] - base, count_in, count_out)
                     for call, stub, count_in, count_out in sites)
        image = native.RoutineImage(base, code, labels[entry] - base, inputs, outputs, rows)
        if publish:
            self.runner.publish(image)
        self.labels = labels
        return image

    def begin(self, image, arguments=(), spans=(), *, frontier=TOP, allowance=1000):
        return self.runner.begin(image, arguments, spans, frontier, FLOOR, allowance)

    def cell(self, address):
        if address >= EXT_BASE:
            offset = address - EXT_BASE
            return int.from_bytes(self.ext[offset:offset + 8], "little")
        return int.from_bytes(self.ram[address:address + 8], "little")


def test_a_routine_returns_its_outputs_and_leaves_the_frontier_where_it_was():
    machine = Machine()
    image = machine.image(ADD, inputs=2, outputs=1)
    event = machine.begin(image, (40, 2))
    assert (event.kind, event.values, event.failure) == ("returned", (42,), None)
    assert event.sp == TOP and event.instructions == 2 and event.cycles > 0
    # The entry sentinel occupied the cell just below the caller's frontier.
    assert machine.cell(TOP - 8) == MASK64
    assert machine.runner.entries == 0
    assert (machine.runner.instructions, machine.runner.segments) == (2, 1)


def test_a_callback_parks_its_return_address_on_the_shared_stack():
    machine = Machine()
    image = machine.image(CALLBACK, inputs=2, outputs=1, sites=(("call", "stub", 2, 1),))
    event = machine.begin(image, (5, 7))
    assert (event.kind, event.values, event.site) == ("callback", (5, 7), 0)
    assert event.image is image
    # Sentinel below the frontier, then the CALL.L return address below it.
    assert event.sp == TOP - 16
    assert machine.cell(TOP - 8) == MASK64
    assert machine.cell(TOP - 16) == machine.labels["after"]
    assert machine.runner.entries == 1 and machine.runner.callbacks == 1
    returned = machine.runner.resume((100,), 1000)
    # The stub's RET.L returns past the CALL.L, which adds the second argument.
    assert (returned.kind, returned.values, returned.sp) == ("returned", (107,), TOP)
    assert machine.runner.entries == 0


def test_a_routine_may_be_entered_again_below_its_own_parked_callback():
    machine = Machine()
    image = machine.image(CALLBACK, inputs=2, outputs=1, sites=(("call", "stub", 2, 1),))
    outer = machine.begin(image, (1, 2))
    inner = machine.begin(image, (10, 20), frontier=outer.sp)
    assert inner.kind == "callback" and inner.sp == outer.sp - 16
    assert machine.cell(outer.sp - 8) == MASK64
    assert machine.runner.entries == 2
    assert machine.runner.resume((30,), 1000).values == (50,)
    # The outer entry's frames were untouched by the nested entry.
    assert machine.cell(outer.sp) == machine.labels["after"]
    finished = machine.runner.resume((3,), 1000)
    assert (finished.kind, finished.values, finished.sp) == ("returned", (5,), TOP)


def test_a_nested_entry_must_start_below_the_parked_frames():
    machine = Machine()
    image = machine.image(CALLBACK, inputs=2, outputs=1, sites=(("call", "stub", 2, 1),))
    outer = machine.begin(image, (1, 2))
    with pytest.raises(ValueError, match="below a parked callback"):
        machine.begin(image, (1, 2), frontier=outer.sp + 8)
    assert machine.runner.entries == 1


def test_a_routine_yields_at_its_allowance_and_continues_exactly():
    machine = Machine()
    image = machine.image(COUNT, inputs=1, outputs=1)
    whole = machine.begin(image, (50,))
    assert whole.kind == "returned"
    first = machine.begin(image, (50,), allowance=7)
    assert (first.kind, first.instructions) == ("yielded", 7)
    assert machine.runner.entries == 1
    total, event = first.instructions, first
    while event.kind == "yielded":
        event = machine.runner.advance(7)
        total += event.instructions
    assert event.kind == "returned" and event.values == (0,)
    assert total == whole.instructions


def test_a_long_segment_lets_other_python_threads_run():
    machine = Machine()
    image = machine.image(COUNT, inputs=1, outputs=1)
    ticks = []
    stop = threading.Event()

    def count():
        while not stop.is_set():
            ticks.append(None)

    worker = threading.Thread(target=count)
    worker.start()
    try:
        before = len(ticks)
        event = machine.begin(image, (3_000_000,), allowance=1 << 40)
        during = len(ticks) - before
    finally:
        stop.set()
        worker.join()
    assert event.kind == "returned" and event.instructions == 6_000_001
    assert during > 0


def test_ordinary_memory_is_reachable_only_through_borrowed_spans():
    machine = Machine()
    image = machine.image(STORE, inputs=2)
    event = machine.begin(image, (DATA, 0x1122), ((DATA, 8, "write"),))
    assert event.kind == "returned" and machine.cell(DATA) == 0x1122
    outside = machine.begin(image, (DATA + 8, 1), ((DATA, 8, "write"),))
    assert (outside.kind, outside.failure) == ("failed", "rejected_access")
    assert (outside.access_address, outside.access_width, outside.access_operation) == (DATA + 8, 8, "write")
    read_only = machine.begin(image, (DATA, 2), ((DATA, 8, "read"),))
    assert read_only.failure == "rejected_access" and machine.cell(DATA) == 0x1122
    assert machine.runner.entries == 0


def test_borrowed_spans_cannot_expose_code_or_the_return_stack():
    machine = Machine()
    image = machine.image(STORE, inputs=2)
    with pytest.raises(ValueError, match="published routine code"):
        machine.begin(image, (CODE, 1), ((CODE, 8, "write"),))
    with pytest.raises(ValueError, match="return stack"):
        machine.begin(image, (TOP - 64, 1), ((TOP - 64, 8, "write"),))
    assert machine.runner.entries == 0


def test_the_machine_stack_is_bounded_by_the_return_stack_floor():
    machine = Machine()
    image = machine.image(CALLBACK, inputs=2, outputs=1, sites=(("call", "stub", 2, 1),))
    with pytest.raises(ValueError, match="no room"):
        machine.begin(image, (1, 2), frontier=FLOOR)
    event = machine.begin(image, (1, 2), frontier=FLOOR + 8)
    assert (event.kind, event.failure) == ("failed", "rejected_access")
    assert event.access_address == FLOOR - 8
    assert "return stack" in event.detail


def test_machine_code_may_call_another_published_routine_directly():
    machine = Machine()
    callee = machine.image("start:\n    inc r4\n    ret.l\n")
    caller = machine.image(f"""
start:
    ldi64 r12, {callee.code_base}
    call.l r12
    ret.l
""", inputs=1, outputs=1)
    assert machine.begin(caller, (41,)).values == (42,)
    machine.runner.revoke(callee)
    assert not machine.runner.is_published(callee)
    event = machine.begin(caller, (41,))
    assert (event.kind, event.failure) == ("failed", "invalid_target")
    assert event.instruction_pc == callee.code_base


def test_a_stub_runs_only_as_the_return_of_its_callback():
    machine = Machine()
    image = machine.image(CALLBACK, inputs=2, outputs=1, entry="stub",
                          sites=(("call", "stub", 2, 1),))
    event = machine.begin(image, (1, 2))
    assert (event.kind, event.failure) == ("failed", "invalid_callback")


def test_only_the_entry_slot_can_return_to_the_semantic_caller():
    machine = Machine()
    image = machine.image(f"""
start:
    ldi64 r12, {MASK64}
    call.l r12
    ret.l
""")
    event = machine.begin(image)
    assert (event.kind, event.failure) == ("failed", "invalid_return")


@pytest.mark.parametrize(("kwargs", "message"), [
    ({"code_base": CODE + 8}, "aligned"),
    ({"code": b"\x01" * 15}, "aligned"),
    ({"input_cells": 9}, "eight cells"),
    ({"entry_offset": 16}, "outside"),
    ({"sites": ((0, 4, 0, 0),)}, "CALL.L"),
    ({"sites": ((0, 1, 0, 0),)}, "overlap"),
])
def test_images_are_checked_when_they_are_made(kwargs, message):
    fields = dict(code_base=CODE, code=b"\x01" * 16, entry_offset=0, input_cells=0,
                  output_cells=0, sites=())
    fields.update(kwargs)
    with pytest.raises(ValueError, match=message):
        native.RoutineImage(**fields)


def test_a_routine_may_not_change_its_stack_pointer_directly():
    raw = bytes(assemble("mov r15, r4\nret.l\n", base_addr=CODE))
    with pytest.raises(ValueError, match="admitted"):
        native.RoutineImage(CODE, raw + b"\x01" * (-len(raw) % 16), 0, 1, 0)


def test_publication_requires_the_bytes_in_memory_and_distinct_code():
    machine = Machine()
    image = machine.image(ADD, inputs=2, outputs=1, publish=False)
    with pytest.raises(ValueError, match="not published"):
        machine.begin(image, (1, 2))
    machine.runner.publish(image)
    with pytest.raises(ValueError, match="overlaps"):
        machine.runner.publish(image)
    copy = native.RoutineImage(image.code_base, image.code, 0, 2, 1)
    with pytest.raises(ValueError, match="overlaps"):
        machine.runner.publish(copy)
    stale = native.RoutineImage(machine.next_code, image.code, 0, 2, 1)
    with pytest.raises(ValueError, match="differs"):
        machine.runner.publish(stale)


def test_a_parked_entry_reserves_the_core_until_it_is_cancelled():
    machine = Machine()
    image = machine.image(CALLBACK, inputs=2, outputs=1, sites=(("call", "stub", 2, 1),))
    machine.begin(image, (1, 2))
    with pytest.raises(RuntimeError, match="hybrid"):
        machine.state.set_reg(4, 0)
    with pytest.raises(ValueError, match="no machine entry has yielded"):
        machine.runner.advance(10)
    machine.runner.cancel()
    assert machine.runner.entries == 0
    machine.state.set_reg(4, 0)
    with pytest.raises(ValueError, match="waiting for a callback"):
        machine.runner.resume((1,), 10)


def test_cancel_keeps_the_requested_number_of_entries():
    machine = Machine()
    image = machine.image(CALLBACK, inputs=2, outputs=1, sites=(("call", "stub", 2, 1),))
    outer = machine.begin(image, (1, 2))
    machine.begin(image, (3, 4), frontier=outer.sp)
    machine.runner.cancel(1)
    assert machine.runner.entries == 1
    assert machine.runner.resume((10,), 1000).values == (12,)


def test_a_closed_runner_rejects_further_work():
    machine = Machine()
    image = machine.image(CALLBACK, inputs=2, outputs=1, sites=(("call", "stub", 2, 1),))
    machine.begin(image, (1, 2))
    machine.runner.close()
    with pytest.raises(RuntimeError, match="closed"):
        machine.begin(image, (1, 2))
    machine.state.set_reg(4, 0)
