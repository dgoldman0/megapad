"""Declared callback segments against real ordinary MP64 CALL.L/RET.L execution."""

from __future__ import annotations

import copy
import pickle
import sys

import _mp64_accel as native
import pytest

from asm import assemble


MASK64 = (1 << 64) - 1
CODE_BASE, RAM_SIZE = 0x100, 4096
EXT_BASE, EXT_SIZE = 0x100000, 256
CONTROL_BASE, CONTROL_SIZE = EXT_BASE + EXT_SIZE, 128
MMIO_BASE, MMIO_LIMIT = 0xFFFFFF0000000000, 0xFFFFFF8000000000

SINGLE = """
    ldi64 r12, stub
    cmp r4, r5
call:
    call.l r12
after:
    ret.l
stub:
    ret.l
"""

DOUBLE = """
    mov r13, r4
    mov r4, r5
    mov r5, r6
    ldi64 r7, 0xAABBCCDDEEFF0011
    ldi64 r12, stub1
    cmp r4, r5
call1:
    call.l r12
after1:
    str r13, r4
    ldi r5, 9
    ldi64 r12, stub2
    cmp r4, r5
call2:
    call.l r12
after2:
    str r13, r4
    ret.l
stub1:
    ret.l
stub2:
    ret.l
"""


class Harness:
    def __init__(self, program=SINGLE, *, callbacks=(("call", "stub", 0, 2, 1),),
                 inputs=2, outputs=1, entry=0, stack_size=CONTROL_SIZE,
                 max_instructions=1000, publish=True):
        self.labels = {}
        raw = bytes(assemble(program, base_addr=CODE_BASE, labels_out=self.labels))
        self.image = raw + b"\x01" * (-len(raw) % 16)
        self.ram, self.external = bytearray(RAM_SIZE), bytearray(EXT_SIZE)
        self.control = bytearray(b"\xA5" * CONTROL_SIZE)
        self.ram[CODE_BASE:CODE_BASE + len(self.image)] = self.image
        self.state = native.CPUState()
        self.state.attach_mem(self.ram, len(self.ram))
        self.state.attach_ext_mem(self.external, EXT_BASE, len(self.external))
        self.state.icache_control_write(1)
        self.runner = native.RoutineRunnerV2(self.state, CONTROL_BASE, self.control)
        self.sites = tuple((self.offset(call), self.offset(stub), export, count_in, count_out)
                           for call, stub, export, count_in, count_out in callbacks)
        self.spec_arguments = (
            CODE_BASE, self.image, self.offset(entry), inputs, outputs,
            CONTROL_BASE, stack_size, max_instructions, self.sites,
        )
        self.spec = native.RoutineSpecV2(*self.spec_arguments)
        self.v1_spec = native.RoutineSpecV1(
            CODE_BASE, len(self.image), self.offset(entry), inputs, outputs,
            CONTROL_BASE, stack_size, max_instructions,
        )
        if publish:
            self.runner.publish_code_v2(self.spec)

    def offset(self, label):
        return self.labels[label] - CODE_BASE if isinstance(label, str) else label

    def begin(self, arguments=(7, 3), spans=(), *, limit=1000, callbacks=1024):
        return self.runner.begin_v2(self.spec, arguments, spans, limit,
                                    callback_limit=callbacks)

    def snapshot(self):
        return (tuple(self.state.get_reg(index) for index in range(32)),
                self.state.flags_pack(), self.state.psel, self.state.xsel, self.state.spsel,
                self.state.cycle_count, self.state.icache_hits, self.state.icache_misses,
                bytes(self.ram), bytes(self.external), bytes(self.control))


class OrdinaryReference:
    """No profile runner: step the existing native interpreter with an ordinary stack mapping."""

    def __init__(self, harness):
        self.harness = harness
        self.ram = bytearray(harness.ram)
        self.external = bytearray(harness.external + harness.control)
        self.state = native.CPUState()
        self.state.attach_mem(self.ram, len(self.ram))
        self.state.attach_ext_mem(self.external, EXT_BASE, len(self.external))
        self.state.icache_control_write(1)
        self.instructions = self.cycles = 0

    def begin(self, arguments):
        for index in range(32):
            self.state.set_reg(index, 0)
        self.state.psel, self.state.xsel, self.state.spsel = 3, 2, 15
        for flag in ("z", "c", "n", "v", "p", "g", "i", "s"):
            setattr(self.state, "flag_" + flag, 0)
        self.state.set_reg(3, CODE_BASE + self.harness.spec_arguments[2])
        top = CONTROL_BASE + self.harness.spec_arguments[6]
        self.state.set_reg(15, top - 8)
        root = top - 8 - EXT_BASE
        self.external[root:root + 8] = MASK64.to_bytes(8, "little")
        for index, value in enumerate(arguments):
            self.state.set_reg(4 + index, value)
        self.instructions = self.cycles = 0

    def segment(self):
        instructions = cycles = 0
        stubs = {CODE_BASE + site[1] for site in self.harness.sites}

        def unexpected(*_args):
            pytest.fail("ordinary integer callback oracle reached a device")

        while True:
            assert self.instructions + instructions < 1000, "oracle did not reach its next boundary"
            cycles += native.step_one(
                self.state, mmio_read8=unexpected, mmio_write8=unexpected,
                on_output=unexpected, csr_read_override=None,
                mmio_start=MMIO_BASE, mmio_end=MMIO_LIMIT,
            )
            instructions += 1
            if self.state.get_reg(3) in stubs or self.state.get_reg(3) == MASK64:
                break
        self.instructions += instructions
        self.cycles += cycles
        return instructions, cycles

    def reply(self, outputs):
        for index, value in enumerate(outputs):
            self.state.set_reg(4 + index, value)

    def assert_equal(self):
        harness = self.harness
        assert tuple(harness.state.get_reg(index) for index in range(32)) == tuple(
            self.state.get_reg(index) for index in range(32)
        )
        assert harness.state.flags_pack() == self.state.flags_pack()
        assert (harness.state.psel, harness.state.xsel, harness.state.spsel) == (3, 2, 15)
        assert bytes(harness.ram) == bytes(self.ram)
        assert bytes(harness.external) == bytes(self.external[:EXT_SIZE])
        assert bytes(harness.control) == bytes(self.external[EXT_SIZE:])


def _signed(value):
    return value - (1 << 64) if value & (1 << 63) else value


def _request(result):
    assert result.exit_kind == "callback_request"
    assert result.callback is not None and result.token is not None
    assert result.outputs == ()
    return result.callback, result.token


def _receipt(receipt):
    return (receipt.segment_id, receipt.invocation_id, receipt.instructions, receipt.cycles,
            receipt.invocation_instructions, receipt.invocation_cycles,
            receipt.callback_request, receipt.invocation_callbacks)


def _assert_receipt_matches(result, receipt, callbacks):
    assert _receipt(receipt) == (
        result.segment_id, result.invocation_id, result.instructions, result.cycles,
        result.invocation_instructions, result.invocation_cycles,
        result.exit_kind == "callback_request", callbacks,
    )


def test_native_receipts_count_each_segment_once_and_remain_readable_after_close():
    harness = Harness(DOUBLE, inputs=3, callbacks=(
        ("call1", "stub1", 7, 2, 1), ("call2", "stub2", 9, 2, 1),
    ))
    assert harness.runner.last_segment_v2() is None
    first = harness.begin((EXT_BASE, 7, 3), [(EXT_BASE, 8, "read_write")])
    _request(first)
    before = harness.snapshot()
    first_receipt = harness.runner.last_segment_v2()
    _assert_receipt_matches(first, first_receipt, 1)
    assert first.segment_id > 0 and harness.snapshot() == before
    with pytest.raises(AttributeError):
        first_receipt.instructions = 0

    second = harness.runner.resume_callback(first.token, (3,))
    _request(second)
    second_receipt = harness.runner.last_segment_v2()
    _assert_receipt_matches(second, second_receipt, 2)
    assert second.segment_id == first.segment_id + 1
    assert second.invocation_id == first.invocation_id
    assert second.invocation_instructions == first.instructions + second.instructions
    assert second.invocation_cycles == first.cycles + second.cycles
    # Receipts are value snapshots, not a view rewritten at the next boundary.
    _assert_receipt_matches(first, first_receipt, 1)

    final = harness.runner.resume_callback(second.token, (9,))
    assert final.exit_kind == "returned" and final.outputs == (9,)
    final_receipt = harness.runner.last_segment_v2()
    _assert_receipt_matches(final, final_receipt, 2)
    assert final.segment_id == second.segment_id + 1
    assert final.invocation_instructions == first.instructions + second.instructions + final.instructions
    assert final.invocation_cycles == first.cycles + second.cycles + final.cycles
    before = harness.snapshot()
    assert harness.runner.cancel_invocation() is None
    assert _receipt(harness.runner.last_segment_v2()) == _receipt(final_receipt)
    assert harness.snapshot() == before
    harness.runner.close()
    assert _receipt(harness.runner.last_segment_v2()) == _receipt(final_receipt)


def test_preflight_failures_and_owner_cancellation_do_not_replace_work_receipt():
    harness = Harness()
    assert harness.runner.cancel_invocation() is None
    before = harness.snapshot()
    with pytest.raises(TypeError):
        harness.begin(callbacks=True)
    assert harness.snapshot() == before
    assert harness.runner.last_segment_v2() is None
    first = harness.begin()
    _request(first)
    receipt = _receipt(harness.runner.last_segment_v2())
    before = harness.snapshot()
    with pytest.raises(TypeError):
        harness.runner.resume_callback(first.token, (True,))
    assert _receipt(harness.runner.last_segment_v2()) == receipt
    assert harness.snapshot() == before
    cancelled = harness.runner.cancel_invocation()
    assert cancelled.exit_kind == "cancelled"
    assert cancelled.instructions == cancelled.cycles == 0
    assert _receipt(harness.runner.last_segment_v2()) == receipt
    assert harness.snapshot() == before
    assert harness.runner.cancel_invocation() is None
    with pytest.raises((ValueError, RuntimeError)):
        harness.runner.cancel_invocation(first.token)
    assert _receipt(harness.runner.last_segment_v2()) == receipt

    harness.ram[CODE_BASE] ^= 1
    before = harness.snapshot()
    with pytest.raises(ValueError):
        harness.begin()
    assert harness.snapshot() == before
    assert _receipt(harness.runner.last_segment_v2()) == receipt
    harness.ram[CODE_BASE] ^= 1
    second = harness.begin()
    assert second.segment_id == first.segment_id + 1
    assert second.invocation_id > first.invocation_id
    _assert_receipt_matches(second, harness.runner.last_segment_v2(), 1)
    harness.runner.cancel_invocation()


def test_zero_work_exhaustion_still_issues_a_unique_terminal_segment_receipt():
    harness = Harness()
    first = harness.begin(limit=3)
    _request(first)
    before = harness.snapshot()
    final = harness.runner.resume_callback(first.token, (99,))
    assert final.exit_kind == "instruction_limit"
    assert final.segment_id == first.segment_id + 1
    assert final.invocation_id == first.invocation_id
    assert final.instructions == final.cycles == 0
    assert final.invocation_instructions == first.invocation_instructions
    assert final.invocation_cycles == first.invocation_cycles
    _assert_receipt_matches(final, harness.runner.last_segment_v2(), 1)
    assert harness.snapshot() == before
    receipt = _receipt(harness.runner.last_segment_v2())
    assert harness.runner.cancel_invocation() is None
    assert _receipt(harness.runner.last_segment_v2()) == receipt


def test_real_calls_returns_registers_private_cells_and_warm_cache_match_ordinary_interpreter():
    harness = Harness(DOUBLE, inputs=3, callbacks=(
        ("call1", "stub1", 7, 2, 1), ("call2", "stub2", 9, 2, 1),
    ))
    reference = OrdinaryReference(harness)
    old_invocations = set()
    warm_misses = None
    for _repeat in range(3):
        arguments = (EXT_BASE, 7, MASK64 - 2)
        reference.begin(arguments)
        result = harness.begin(arguments, [(EXT_BASE, 8, "read_write")])
        invocation = result.invocation_id
        assert invocation > 0 and invocation not in old_invocations
        old_invocations.add(invocation)
        sequence = 0
        while True:
            instructions, cycles = reference.segment()
            assert (result.instructions, result.cycles) == (instructions, cycles)
            assert (result.invocation_instructions, result.invocation_cycles) == (
                reference.instructions, reference.cycles,
            )
            assert result.invocation_id == invocation
            reference.assert_equal()
            if result.exit_kind == "returned":
                assert result.outputs == (9,)
                assert result.callback is None and result.token is None
                break
            callback, token = _request(result)
            site = harness.sites[sequence]
            sequence += 1
            assert (callback.sequence, callback.call_offset, callback.stub_offset,
                    callback.export_id) == (sequence, site[0], site[1], site[2])
            assert callback.arguments == tuple(reference.state.get_reg(4 + index)
                                               for index in range(site[3]))
            assert result.pc == CODE_BASE + site[1]
            slot = harness.state.get_reg(15) - CONTROL_BASE
            assert int.from_bytes(harness.control[slot:slot + 8], "little") == CODE_BASE + site[0] + 2
            operation = min if callback.export_id == 7 else max
            output = operation(map(_signed, callback.arguments)) & MASK64
            reference.reply((output,))
            result = harness.runner.resume_callback(token, (output,))
        assert sequence == 2
        assert int.from_bytes(harness.external[:8], "little") == 9
        if warm_misses is not None:
            assert harness.state.icache_misses == warm_misses
        warm_misses = harness.state.icache_misses


def test_return_on_final_allowance_succeeds_without_resetting_segment_budget():
    harness = Harness(max_instructions=5)
    request = harness.begin(limit=5)
    _callback, token = _request(request)
    assert request.instructions == request.invocation_instructions == 3
    result = harness.runner.resume_callback(token, (3,))
    assert result.exit_kind == "returned" and result.outputs == (3,)
    assert result.instructions == 2 and result.invocation_instructions == 5
    assert result.invocation_cycles == request.cycles + result.cycles

    request = harness.begin(limit=4)
    _callback, token = _request(request)
    result = harness.runner.resume_callback(token, (3,))
    assert result.exit_kind == "instruction_limit" and result.outputs == ()
    assert result.instructions == 1 and result.invocation_instructions == 4
    assert result.pc == harness.labels["after"]
    assert harness.state.get_reg(15) == CONTROL_BASE + CONTROL_SIZE - 8


def test_callback_call_on_final_instruction_parks_but_resume_applies_no_outputs():
    harness = Harness()
    request = harness.begin(limit=3)
    _callback, token = _request(request)
    before = harness.snapshot()
    result = harness.runner.resume_callback(token, (99,))
    assert result.exit_kind == "instruction_limit"
    assert result.instructions == result.cycles == 0
    assert result.invocation_instructions == 3
    assert result.invocation_cycles == request.cycles
    assert result.callback is None and result.token is None
    assert harness.snapshot() == before
    with pytest.raises((ValueError, RuntimeError)):
        harness.runner.resume_callback(token, (99,))


def test_declared_local_limit_is_not_renewed_by_a_larger_begin_allowance():
    harness = Harness(max_instructions=4)
    request = harness.begin(limit=1000)
    _callback, token = _request(request)
    result = harness.runner.resume_callback(token, (3,))
    assert result.exit_kind == "instruction_limit"
    assert (request.instructions, result.instructions, result.invocation_instructions) == (3, 1, 4)
    assert result.pc == harness.labels["after"]


@pytest.mark.parametrize("arity", (0, 8))
def test_callback_argument_and_output_extremes_use_only_declared_register_cells(arity):
    harness = Harness(inputs=arity, outputs=arity,
                      callbacks=(("call", "stub", 63, arity, arity),))
    arguments = tuple((MASK64 - index) for index in range(arity))
    callback, token = _request(harness.begin(arguments))
    assert callback.arguments == arguments
    flags = harness.state.flags_pack()
    target = harness.state.get_reg(12)
    output = tuple(reversed(arguments))
    result = harness.runner.resume_callback(token, output)
    assert result.exit_kind == "returned" and result.outputs == output
    assert harness.state.flags_pack() == flags
    assert harness.state.get_reg(12) == target


@pytest.mark.parametrize("limit,error", ((-1, ValueError), (1025, ValueError), (True, TypeError), (1.0, TypeError)))
def test_callback_limit_preflight_has_no_entry_effects(limit, error):
    harness = Harness()
    before = harness.snapshot()
    with pytest.raises(error):
        harness.begin(callbacks=limit)
    assert harness.snapshot() == before


def test_zero_remaining_callbacks_preserves_real_call_prefix_but_allows_callback_free_return():
    harness = Harness()
    reference = OrdinaryReference(harness)
    reference.begin((7, 3))
    result = harness.begin(callbacks=0)
    instructions, cycles = reference.segment()
    assert result.exit_kind == "callback_limit"
    assert (result.instructions, result.cycles) == (instructions, cycles)
    assert result.instructions == 3 and result.pc == harness.labels["stub"]
    assert result.callback is None and result.token is None
    reference.assert_equal()

    callback_free = Harness("ret.l\ncall:\n call.l r4\nstub:\n ret.l")
    result = callback_free.begin(callbacks=0)
    assert result.exit_kind == "returned" and result.outputs == (7,)
    assert result.instructions == 1


def test_local_instruction_and_callback_allowances_cover_all_segments():
    program = """
        ldi r6, 3
        ldi64 r12, stub
    loop:
    call:
        call.l r12
        subi r6, 1
        brne loop
        ret.l
    stub:
        ret.l
    """
    harness = Harness(program)
    result = harness.begin(callbacks=2)
    invocation = result.invocation_id
    for sequence in (1, 2):
        callback, token = _request(result)
        assert callback.sequence == sequence
        result = harness.runner.resume_callback(token, (sequence,))
    assert result.exit_kind == "callback_limit"
    assert result.invocation_id == invocation
    assert result.callback is None and result.token is None
    assert result.invocation_instructions == 11
    assert result.pc == harness.labels["stub"]
    assert harness.state.get_reg(15) == CONTROL_BASE + CONTROL_SIZE - 16


@pytest.mark.parametrize("program,entry,expected_instructions", (
    ("call:\n call.l r4\n stub:\n ret.l", "stub", 0),
    ("br stub\n call:\n call.l r4\n stub:\n ret.l", 0, 1),
    ("entry:\n nop\n stub:\n ret.l\n call:\n call.l r4", "entry", 1),
    ("call.l r4\n ret.l\n call:\n call.l r4\n stub:\n ret.l", 0, 1),
))
def test_stub_entry_requires_its_exact_declared_completed_call(program, entry, expected_instructions):
    harness = Harness(program, entry=entry)
    result = harness.begin((harness.labels["stub"], 0))
    assert result.exit_kind == "invalid_callback"
    assert result.instructions == expected_instructions
    assert result.callback is None and result.token is None
    assert result.outputs == ()


def test_wrong_target_preserves_completed_call_push_and_failed_call_never_requests():
    program = "call:\n call.l r4\n after:\n ret.l\n stub:\n ret.l"
    harness = Harness(program)
    result = harness.begin((harness.labels["after"], 0))
    assert result.exit_kind == "invalid_callback" and result.instructions == 1
    assert result.pc == harness.labels["after"]
    assert result.callback is None and result.token is None
    slot = CONTROL_SIZE - 16
    assert int.from_bytes(harness.control[slot:slot + 8], "little") == harness.labels["after"]

    shallow = Harness(program, stack_size=8)
    result = shallow.begin((shallow.labels["stub"], 0))
    assert result.exit_kind == "rejected_access" and result.instructions == 0
    assert result.access_address == CONTROL_BASE - 8
    assert shallow.state.get_reg(15) == CONTROL_BASE - 8
    assert result.callback is None and result.token is None


@pytest.mark.parametrize("outputs,error", (
    ((), ValueError), ((1, 2), ValueError), ((True,), TypeError),
    ((-1,), ValueError), ((1 << 64,), ValueError), ((1.0,), TypeError),
    (("1",), TypeError), (iter((1,)), TypeError),
))
def test_malformed_reply_preserves_pending_state_and_can_be_retried(outputs, error):
    harness = Harness()
    _callback, token = _request(harness.begin())
    before = harness.snapshot()
    with pytest.raises(error):
        harness.runner.resume_callback(token, outputs)
    assert harness.snapshot() == before
    assert harness.runner.resume_callback(token, (3,)).outputs == (3,)


def test_foreign_replayed_and_previous_invocation_tokens_do_not_consume_current_request():
    first, second = Harness(), Harness()
    one, two = first.begin(), second.begin()
    _request(one)
    _request(two)
    before = first.snapshot(), second.snapshot()
    with pytest.raises((ValueError, RuntimeError)):
        second.runner.resume_callback(one.token, (3,))
    assert (first.snapshot(), second.snapshot()) == before
    assert first.runner.resume_callback(one.token, (3,)).exit_kind == "returned"
    current = first.begin()
    assert current.invocation_id != one.invocation_id
    before = first.snapshot()
    with pytest.raises((ValueError, RuntimeError)):
        first.runner.resume_callback(one.token, (3,))
    assert first.snapshot() == before
    assert first.runner.resume_callback(current.token, (3,)).exit_kind == "returned"
    assert second.runner.resume_callback(two.token, (3,)).exit_kind == "returned"


@pytest.mark.parametrize("copier", (copy.copy, copy.deepcopy, pickle.dumps))
def test_continuation_tokens_cannot_be_copied_or_serialized(copier):
    harness = Harness()
    _callback, token = _request(harness.begin())
    before = harness.snapshot()
    with pytest.raises(TypeError):
        copier(token)
    assert harness.snapshot() == before
    assert harness.runner.resume_callback(token, (3,)).exit_kind == "returned"


def test_forged_return_cell_and_changed_seal_reject_reply_before_outputs():
    for mutation in ("return_cell", "code"):
        harness = Harness()
        _callback, token = _request(harness.begin())
        if mutation == "return_cell":
            slot = harness.state.get_reg(15) - CONTROL_BASE
            harness.control[slot:slot + 8] = MASK64.to_bytes(8, "little")
        else:
            harness.ram[harness.labels["stub"]] = 0x01
        before = harness.snapshot()
        with pytest.raises((ValueError, RuntimeError)):
            harness.runner.resume_callback(token, (99,))
        assert harness.snapshot() == before
        harness.runner.cancel_invocation()


@pytest.mark.parametrize("operation", ("begin", "v1_run", "publish", "v1_publish", "revoke", "query", "register", "flags", "icache", "device", "step", "run"))
def test_parked_owner_blocks_execution_publication_and_cpu_mutation(operation):
    harness = Harness()
    _callback, token = _request(harness.begin())
    before = harness.snapshot()

    def unexpected(*_args):
        pytest.fail("parked CPU reached a device callback")

    execution_arguments = dict(
        mmio_read8=unexpected, mmio_write8=unexpected, on_output=unexpected,
        csr_read_override=None, mmio_start=MMIO_BASE, mmio_end=MMIO_LIMIT,
    )
    actions = {
        "begin": harness.begin,
        "v1_run": lambda: harness.runner.run(harness.v1_spec, (7, 3), (), 100),
        "publish": lambda: harness.runner.publish_code_v2(harness.spec),
        "v1_publish": lambda: harness.runner.publish_code(harness.v1_spec),
        "revoke": lambda: harness.runner.revoke_code_v2(harness.spec),
        "query": lambda: harness.runner.is_code_published_v2(harness.spec),
        "register": lambda: harness.state.set_reg(4, 99),
        "flags": lambda: setattr(harness.state, "flag_z", 1),
        "icache": lambda: harness.state.icache_control_write(0),
        "device": lambda: harness.state.init_crypto(),
        "step": lambda: native.step_one(harness.state, **execution_arguments),
        "run": lambda: native.run_steps(harness.state, max_steps=1, **execution_arguments),
    }
    with pytest.raises(RuntimeError):
        actions[operation]()
    assert harness.snapshot() == before
    assert harness.runner.resume_callback(token, (3,)).exit_kind == "returned"


def test_cancel_releases_parked_owner_without_reversing_completed_stores():
    harness = Harness("st.b r6, r5\n" + SINGLE, inputs=3)
    request = harness.begin((7, 3, EXT_BASE), [(EXT_BASE, 1, "write")])
    _request(request)
    before = harness.snapshot()
    result = harness.runner.cancel_invocation(request.token)
    assert result.exit_kind == "cancelled"
    assert result.instructions == result.cycles == 0
    assert result.invocation_instructions == request.invocation_instructions
    assert result.invocation_cycles == request.invocation_cycles
    assert harness.snapshot() == before
    assert harness.external[0] == 3
    with pytest.raises((ValueError, RuntimeError)):
        harness.runner.resume_callback(request.token, (99,))
    assert harness.snapshot() == before
    next_request = harness.begin((7, 3, EXT_BASE), [(EXT_BASE, 1, "write")])
    assert next_request.invocation_id != request.invocation_id
    assert harness.runner.resume_callback(next_request.token, (2,)).outputs == (2,)


def test_close_revokes_pending_reply_and_releases_private_buffer_pin():
    harness = Harness()
    request = harness.begin()
    before = harness.snapshot()
    harness.runner.close()
    with pytest.raises(RuntimeError):
        harness.runner.resume_callback(request.token, (99,))
    assert harness.snapshot() == before
    harness.control.extend(b"released")
    harness.state.attach_mem(bytearray(harness.ram), RAM_SIZE)


def test_equal_unpublished_spec_clone_and_changed_source_cannot_begin():
    harness = Harness()
    clone = native.RoutineSpecV2(*harness.spec_arguments)
    before = harness.snapshot()
    with pytest.raises((ValueError, RuntimeError)):
        harness.runner.begin_v2(clone, (7, 3), (), 100)
    assert harness.snapshot() == before
    harness.ram[CODE_BASE] ^= 1
    before = harness.snapshot()
    with pytest.raises((ValueError, RuntimeError)):
        harness.begin()
    assert harness.snapshot() == before


def test_stale_resident_code_cache_requires_explicit_republication_before_entry():
    harness = Harness()
    request = harness.begin()
    assert harness.runner.resume_callback(request.token, (3,)).exit_kind == "returned"
    valid, tags, payload = harness.state.icache_snapshot()
    stale = bytearray(payload)
    line = (CODE_BASE >> 4) & 0xFF
    stale[line * 16] ^= 1
    stale_cache = (valid, tags, bytes(stale))
    harness.state.icache_restore(*stale_cache)
    before = harness.snapshot()
    with pytest.raises(ValueError, match="cache|I-cache"):
        harness.begin()
    assert harness.snapshot() == before
    assert harness.state.icache_snapshot() == stale_cache
    assert bytes(harness.ram[CODE_BASE:CODE_BASE + len(harness.image)]) == harness.image
    harness.runner.publish_code_v2(harness.spec)
    request = harness.begin()
    assert harness.runner.resume_callback(request.token, (2,)).outputs == (2,)


@pytest.mark.parametrize("fault", ("duplicate_call", "shared_stub", "wrong_call", "wrong_stub", "immediate_call", "immediate_stub", "entry_inside_operand"))
def test_publication_proves_instruction_boundaries_and_disjoint_canonical_sites(fault):
    harness = Harness(DOUBLE, inputs=3, callbacks=(
        ("call1", "stub1", 7, 2, 1), ("call2", "stub2", 9, 2, 1),
    ), publish=False)
    image = bytearray(harness.image)
    arguments = list(harness.spec_arguments)
    sites = [list(site) for site in harness.sites]
    if fault == "duplicate_call":
        sites[1][0] = sites[0][0]
    elif fault == "shared_stub":
        sites[1][1] = sites[0][1]
    elif fault == "wrong_call":
        sites[0][0] = 0  # MOV, not CALL.L.
    elif fault == "wrong_stub":
        sites[0][1] = 0
    else:
        # The first LDI64 immediate follows three MOVs and its three-byte
        # header. Its bytes may spell real opcodes without being boundaries.
        immediate = len(assemble("mov r13, r4\nmov r4, r5\nmov r5, r6")) + 3
        if fault == "immediate_call":
            image[immediate:immediate + 2] = assemble("call.l r12")
            sites[0][0] = immediate
        elif fault == "immediate_stub":
            image[immediate] = assemble("ret.l")[0]
            sites[0][1] = immediate
        else:
            arguments[2] = immediate
    arguments[1] = bytes(image)
    arguments[-1] = tuple(tuple(site) for site in sites)
    harness.ram[CODE_BASE:CODE_BASE + len(image)] = image
    before = harness.snapshot()
    with pytest.raises(ValueError):
        malformed = native.RoutineSpecV2(*arguments)
        harness.runner.publish_code_v2(malformed)
    assert harness.snapshot() == before


def test_callback_call_with_rex_prefix_is_not_a_canonical_two_byte_site():
    # R20 requires REX before CALL.L; a normal admitted machine call may use
    # that register, but it cannot gain callback authority through this site.
    with pytest.raises(ValueError):
        Harness("mov r20, r4\ncall:\n call.l r20\nret.l\nstub:\n ret.l")


def test_v1_remains_distinct_and_calls_the_same_stub_as_ordinary_machine_code():
    harness = Harness()
    with pytest.raises(TypeError):
        harness.runner.run(harness.spec, (7, 3), (), 100)
    result = harness.runner.run(harness.v1_spec, (7, 3), (), 100)
    assert result.exit_kind == "returned" and result.outputs == (7,)
    request = harness.begin()
    _request(request)
    assert harness.runner.resume_callback(request.token, (3,)).outputs == (3,)


@pytest.mark.parametrize("operation", ("publish", "begin", "revoke", "query"))
def test_none_spec_is_rejected_before_native_owner_or_machine_effects(operation):
    harness = Harness()
    before = harness.snapshot()
    actions = {
        "publish": lambda: harness.runner.publish_code_v2(None),
        "begin": lambda: harness.runner.begin_v2(None, (), (), 1),
        "revoke": lambda: harness.runner.revoke_code_v2(None),
        "query": lambda: harness.runner.is_code_published_v2(None),
    }
    with pytest.raises(TypeError):
        actions[operation]()
    assert harness.snapshot() == before
    assert harness.runner.is_code_published_v2(harness.spec)


def test_revocation_removes_exact_publication_and_republication_does_not_revive_old_token():
    harness = Harness()
    old = harness.begin()
    harness.runner.cancel_invocation(old.token)
    assert harness.runner.is_code_published_v2(harness.spec)
    harness.runner.revoke_code_v2(harness.spec)
    assert not harness.runner.is_code_published_v2(harness.spec)
    before = harness.snapshot()
    with pytest.raises(ValueError):
        harness.begin()
    assert harness.snapshot() == before
    harness.runner.publish_code_v2(harness.spec)
    current = harness.begin()
    with pytest.raises(ValueError):
        harness.runner.resume_callback(old.token, (99,))
    assert harness.runner.resume_callback(current.token, (3,)).outputs == (3,)


@pytest.mark.skipif(sys.version_info < (3, 12), reason="requires Python buffer export callbacks")
def test_attachment_exporter_cannot_publish_runner_while_mapping_is_being_replaced():
    state = native.CPUState()
    state.attach_mem(bytearray(RAM_SIZE), RAM_SIZE)
    failures, created = [], []

    class Exporter:
        def __init__(self):
            self.storage = bytearray(RAM_SIZE)

        def __buffer__(self, _flags):
            try:
                created.append(native.RoutineRunnerV2(state, CONTROL_BASE, bytearray(CONTROL_SIZE)))
            except RuntimeError as error:
                failures.append(error)
            return memoryview(self.storage)

    exporter = Exporter()
    state.attach_mem(exporter, RAM_SIZE)
    assert created == [] and len(failures) >= 1
    runner = native.RoutineRunnerV2(state, CONTROL_BASE, bytearray(CONTROL_SIZE))
    runner.close()
    state.attach_mem(bytearray(RAM_SIZE), RAM_SIZE)
