"""Root-only V3 segments retain real MP64 effects and independent accounting."""

from __future__ import annotations

import copy
import pickle

import _mp64_accel as native
import pytest

from asm import assemble


MASK64 = (1 << 64) - 1
CODE_BASE, RAM_SIZE = 0x100, 4096
EXT_BASE, EXT_SIZE = 0x100000, 256
CONTROL_BASE, CONTROL_SIZE = EXT_BASE + EXT_SIZE, 128
MMIO_BASE, MMIO_END = 0xFFFFFF0000000000, 0xFFFFFF8000000000
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
BUFFER = """
    mov r13, r4
    mov r4, r5
    mov r5, r6
    ldi64 r10, 0xAABBCCDDEEFF0011
    ldi64 r12, stub
    cmp r4, r5
call:
    call.l r12
after:
    str r13, r4
    ret.l
stub:
    ret.l
"""
LOOP = """
    ldi r6, 2
    ldi64 r12, stub
call:
    call.l r12
after:
    subi r6, 1
    brne call
    ret.l
stub:
    ret.l
"""
RECEIPT_FIELDS = (
    "segment_id", "root_invocation_id", "invocation_id", "parent_invocation_id", "depth",
    "invocation_started", "instructions", "cycles", "invocation_instructions", "invocation_cycles",
    "invocation_callbacks", "chain_instructions", "chain_cycles", "chain_callbacks", "callback_request",
)


class Harness:
    def __init__(self, source=SINGLE, *, inputs=2, outputs=1, entry=0,
                 max_instructions=100, max_callback_requests=1, stack_size=CONTROL_SIZE,
                 callbacks=(("call", "stub", 7, 2, 1),)):
        self.labels = {}
        raw = bytes(assemble(source, base_addr=CODE_BASE, labels_out=self.labels))
        self.code = raw + b"\x01" * (-len(raw) % 16)
        self.ram, self.external = bytearray(RAM_SIZE), bytearray(EXT_SIZE)
        self.control = bytearray(b"\xA5" * CONTROL_SIZE)
        self.ram[CODE_BASE:CODE_BASE + len(self.code)] = self.code
        self.state = native.CPUState()
        self.state.attach_mem(self.ram, len(self.ram))
        self.state.attach_ext_mem(self.external, EXT_BASE, EXT_SIZE)
        self.state.icache_control_write(1)
        self.runner = native.RoutineRunnerV3(self.state, CONTROL_BASE, self.control)
        self.legacy = self.runner.legacy_v2()
        self.sites = tuple((self.offset(call), self.offset(stub), export, count_in, count_out)
                           for call, stub, export, count_in, count_out in callbacks)
        self.fields = dict(code_base=CODE_BASE, code=self.code, entry_offset=self.offset(entry),
                           input_cells=inputs, output_cells=outputs, stack_base=CONTROL_BASE,
                           stack_size=stack_size, max_instructions=max_instructions,
                           max_callback_requests=max_callback_requests, callbacks=self.sites)
        self.spec = native.RoutineSpecV3(**self.fields)
        self.v2 = native.RoutineSpecV2(**{name: value for name, value in self.fields.items()
                                        if name != "max_callback_requests"})
        self.v1 = native.RoutineSpecV1(CODE_BASE, len(self.code), self.offset(entry), inputs, outputs,
                                      CONTROL_BASE, stack_size, max_instructions)
        self.runner.publish_code_v3(self.spec)

    def offset(self, value):
        return self.labels[value] - CODE_BASE if isinstance(value, str) else value

    def begin(self, arguments=(7, 3), spans=(), *, instructions=100, callbacks=1024, protected=()):
        return self.runner.begin_root_v3(self.spec, arguments, spans, instructions,
                                         callback_limit=callbacks, protected_spans=protected)

    def snapshot(self):
        return (tuple(self.state.get_reg(index) for index in range(32)), self.state.flags_pack(),
                self.state.psel, self.state.xsel, self.state.spsel, self.state.cycle_count,
                self.state.icache_hits, self.state.icache_misses, self.state.icache_snapshot(),
                bytes(self.ram), bytes(self.external), bytes(self.control))


class OrdinaryReference:
    """Existing native CPU stepping, with the private control bytes mapped ordinarily."""

    def __init__(self, harness):
        self.harness = harness
        self.ram = bytearray(harness.ram)
        self.memory = bytearray(harness.external + harness.control)
        self.state = native.CPUState()
        self.state.attach_mem(self.ram, len(self.ram))
        self.state.attach_ext_mem(self.memory, EXT_BASE, len(self.memory))
        self.state.icache_control_write(1)
        self.instructions = self.cycles = 0

    def begin(self, arguments):
        for index in range(32):
            self.state.set_reg(index, 0)
        self.state.psel, self.state.xsel, self.state.spsel = 3, 2, 15
        self.state.sw = 1
        self.state.flags_unpack(0)
        self.state.d_reg = self.state.q_out = self.state.t_reg = self.state.ef_flags = 0
        self.state.halted = self.state.idle = False
        # CPUState() value-initializes this field to 0 (EXT.IMM64), whereas
        # routine entry starts without a pending instruction prefix.
        self.state.ext_modifier = -1
        self.state.ivt_base = self.state.ivec_id = self.state.trap_addr = self.state.wake_ms = 0
        self.state.priv_level = self.state.core_id = 0
        self.state.num_cores = 1
        self.state.irq_ipi = False
        self.state.icache_enabled = 1
        self.state.set_reg(3, CODE_BASE + self.harness.spec.entry_offset)
        top = CONTROL_BASE + self.harness.spec.stack_size
        self.state.set_reg(15, top - 8)
        self.memory[top - 8 - EXT_BASE:top - EXT_BASE] = MASK64.to_bytes(8, "little")
        for index, argument in enumerate(arguments):
            self.state.set_reg(4 + index, argument)
        self.instructions = self.cycles = 0

    def step(self):
        def unexpected(*_args):
            pytest.fail("ordinary integer oracle reached a device")

        cycles = native.step_one(self.state, mmio_read8=unexpected, mmio_write8=unexpected,
                                  on_output=unexpected, csr_read_override=None,
                                  mmio_start=MMIO_BASE, mmio_end=MMIO_END)
        self.instructions += 1
        self.cycles += cycles
        return cycles

    def segment(self):
        before = self.instructions, self.cycles
        stubs = {CODE_BASE + site[1] for site in self.harness.sites}
        while True:
            assert self.instructions < 100, "ordinary reference did not reach its boundary"
            self.step()
            if self.state.get_reg(3) in stubs or self.state.get_reg(3) == MASK64:
                return self.instructions - before[0], self.cycles - before[1]

    def reply(self, values):
        for index, value in enumerate(values):
            self.state.set_reg(4 + index, value)

    def assert_equal(self):
        harness = self.harness
        assert tuple(harness.state.get_reg(index) for index in range(32)) == tuple(
            self.state.get_reg(index) for index in range(32))
        assert harness.state.flags_pack() == self.state.flags_pack()
        assert (harness.state.psel, harness.state.xsel, harness.state.spsel) == (3, 2, 15)
        assert bytes(harness.ram) == bytes(self.ram)
        assert bytes(harness.external) == bytes(self.memory[:EXT_SIZE])
        assert bytes(harness.control) == bytes(self.memory[EXT_SIZE:])
        assert harness.state.cycle_count == self.state.cycle_count
        assert (harness.state.icache_hits, harness.state.icache_misses) == (
            self.state.icache_hits, self.state.icache_misses)


def _receipt(value):
    return tuple(getattr(value, name) for name in RECEIPT_FIELDS)


def _assert_root_receipt(harness, result, *, started, callbacks):
    receipt = harness.runner.last_segment_v3()
    assert type(receipt) is native.RoutineSegmentReceiptV3
    assert (result.root_invocation_id, result.parent_invocation_id, result.depth) == (
        result.invocation_id, 0, 1)
    assert result.invocation_started is started
    assert result.invocation_callbacks == result.chain_callbacks == callbacks
    assert result.chain_instructions == result.invocation_instructions
    assert result.chain_cycles == result.invocation_cycles
    assert _receipt(receipt) == tuple(
        result.exit_kind == "callback_request" if name == "callback_request" else getattr(result, name)
        for name in RECEIPT_FIELDS)
    assert not hasattr(receipt, "token") and not hasattr(receipt, "outputs")
    return receipt


def _request(result):
    assert result.exit_kind == "callback_request"
    assert type(result) is native.RoutineSegmentResultV3
    assert not isinstance(result, native.RoutineSegmentResultV2)
    assert type(result.callback) is native.RoutineCallbackRequestV3
    assert type(result.token) is native.RoutineCallbackTokenV3
    assert result.callback.site_index == 0 and result.outputs == ()
    return result.callback, result.token


def test_root_segments_match_ordinary_calls_returns_registers_control_bytes_and_cache():
    harness = Harness(BUFFER, inputs=3)
    reference = OrdinaryReference(harness)
    previous_invocation = 0
    for _repeat in range(2):
        arguments = (EXT_BASE, 7, MASK64 - 2)
        reference.begin(arguments)
        first = harness.begin(arguments, ((EXT_BASE, 8, "read_write"),))
        assert first.invocation_id > previous_invocation
        previous_invocation = first.invocation_id
        callback, token = _request(first)
        assert (first.instructions, first.cycles) == reference.segment()
        assert first.instructions == 7
        assert callback.arguments == (7, MASK64 - 2)
        assert (callback.sequence, callback.call_offset, callback.stub_offset, callback.export_id) == (
            1, harness.sites[0][0], harness.sites[0][1], 7)
        reference.assert_equal()
        slot = harness.state.get_reg(15) - CONTROL_BASE
        assert int.from_bytes(harness.control[slot:slot + 8], "little") == harness.labels["after"]
        first_receipt = _assert_root_receipt(harness, first, started=True, callbacks=1)
        reference.reply((MASK64 - 2,))
        final = harness.runner.resume_callback_v3(token, (MASK64 - 2,))
        assert (final.instructions, final.cycles) == reference.segment()
        assert final.instructions == 3 and final.invocation_instructions == 10
        assert final.exit_kind == "returned" and final.outputs == (MASK64 - 2,)
        assert final.segment_id == first.segment_id + 1
        assert final.callback is None and final.token is None
        reference.assert_equal()
        _assert_root_receipt(harness, final, started=False, callbacks=1)
        assert first_receipt.instructions == 7 and first_receipt.invocation_started is True
        assert int.from_bytes(harness.external[:8], "little") == MASK64 - 2
    assert type(native.HYBRID_NESTED_ROUTINE_ABI_VERSION) is int
    assert native.HYBRID_NESTED_ROUTINE_ABI_VERSION == 3
    assert type(native.HYBRID_NESTED_ROUTINE_CAPABILITY) is str
    assert native.HYBRID_NESTED_ROUTINE_CAPABILITY == "distinct_registration_children"
    assert type(native.HYBRID_NESTED_ROUTINE_MAX_DEPTH) is int
    assert native.HYBRID_NESTED_ROUTINE_MAX_DEPTH == 8
    harness.runner.close()


@pytest.mark.parametrize("local,root,kind,own", [(5, 5, "returned", 5),
    (100, 4, "instruction_limit", 4), (4, 100, "instruction_limit", 4)])
def test_local_and_root_instruction_limits_are_not_renewed_on_resume(local, root, kind, own):
    harness = Harness(max_instructions=local)
    first = harness.begin(instructions=root)
    _request(first)
    final = harness.runner.resume_callback_v3(first.token, (3,))
    assert final.exit_kind == kind and final.invocation_instructions == own
    assert (first.instructions, final.instructions) == (3, own - 3)
    assert final.invocation_cycles == first.cycles + final.cycles
    assert final.outputs == ((3,) if kind == "returned" else ())
    _assert_root_receipt(harness, final, started=False, callbacks=1)
    harness.runner.close()


def test_last_instruction_call_parks_and_zero_work_resume_consumes_token_without_outputs():
    harness = Harness()
    first = harness.begin(instructions=3)
    _request(first)
    before = harness.snapshot()
    final = harness.runner.resume_callback_v3(first.token, (99,))
    assert final.exit_kind == "instruction_limit"
    assert final.instructions == final.cycles == 0 and final.outputs == ()
    assert final.segment_id == first.segment_id + 1
    assert final.invocation_instructions == first.invocation_instructions == 3
    assert harness.snapshot() == before
    receipt = _assert_root_receipt(harness, final, started=False, callbacks=1)
    assert harness.runner.cancel_chain_v3() is None
    assert _receipt(harness.runner.last_segment_v3()) == _receipt(receipt)
    with pytest.raises((ValueError, RuntimeError)):
        harness.runner.resume_callback_v3(first.token, (99,))
    harness.runner.close()


@pytest.mark.parametrize("local,root", [(0, 1024), (1, 0)])
def test_zero_local_or_root_callback_allowance_retains_completed_call_and_no_request(local, root):
    harness = Harness(max_callback_requests=local)
    reference = OrdinaryReference(harness)
    reference.begin((7, 3))
    result = harness.begin(callbacks=root)
    assert (result.instructions, result.cycles) == reference.segment()
    assert result.exit_kind == "callback_limit" and result.instructions == 3
    assert result.callback is None and result.token is None and result.outputs == ()
    reference.assert_equal()
    _assert_root_receipt(harness, result, started=True, callbacks=0)
    harness.runner.close()


def test_zero_callback_allowance_still_permits_callback_free_path():
    harness = Harness("ret.l\ncall:\n call.l r4\nstub:\n ret.l", max_callback_requests=0)
    result = harness.begin(callbacks=0)
    assert result.exit_kind == "returned" and result.outputs == (7,) and result.instructions == 1
    _assert_root_receipt(harness, result, started=True, callbacks=0)
    harness.runner.close()


@pytest.mark.parametrize("local,root", [(1, 1024), (2, 1)])
def test_callback_allowance_covers_repeated_segments_of_the_same_frame(local, root):
    harness = Harness(LOOP, max_callback_requests=local)
    first = harness.begin(callbacks=root)
    _request(first)
    final = harness.runner.resume_callback_v3(first.token, (3,))
    assert final.exit_kind == "callback_limit" and final.invocation_instructions == 7
    assert final.instructions == 4 and final.pc == harness.labels["stub"]
    assert final.callback is None and final.token is None and final.outputs == ()
    _assert_root_receipt(harness, final, started=False, callbacks=1)
    harness.runner.close()


@pytest.mark.parametrize("change", [dict(instructions=0), dict(instructions=True),
    dict(callbacks=True), dict(callbacks=-1), dict(callbacks=1025), dict(arguments=(True, 3)),
    dict(arguments=(7,)), dict(spans=((EXT_BASE + EXT_SIZE - 1, 2, "read"),)),
    dict(spans=((CODE_BASE, 1, "read"),)),
    dict(spans=((EXT_BASE, 8, "read"),), protected=((EXT_BASE, 8),))])
def test_rejected_root_preflight_has_no_state_or_receipt_effects(change):
    harness = Harness()
    before = harness.snapshot()
    with pytest.raises((TypeError, ValueError)):
        harness.begin(**change)
    assert harness.snapshot() == before and harness.runner.last_segment_v3() is None
    first = harness.begin()
    assert first.segment_id == first.invocation_id == 1
    harness.runner.cancel_chain_v3()
    harness.runner.close()


def test_wrong_spec_and_missing_token_types_cannot_enter_or_replace_a_root():
    harness = Harness()
    before = harness.snapshot()
    for spec in (None, harness.v1, harness.v2, object()):
        with pytest.raises(TypeError):
            harness.runner.begin_root_v3(spec, (7, 3), (), 100)
        assert harness.snapshot() == before and harness.runner.last_segment_v3() is None
    first = harness.begin()
    before, receipt = harness.snapshot(), _receipt(harness.runner.last_segment_v3())
    for token in (None, object()):
        with pytest.raises(TypeError):
            harness.runner.resume_callback_v3(token, (3,))
        assert harness.snapshot() == before and _receipt(harness.runner.last_segment_v3()) == receipt
    assert harness.runner.resume_callback_v3(first.token, (3,)).outputs == (3,)
    harness.runner.close()


def test_zero_work_accepted_fault_has_receipt_where_preflight_rejection_does_not():
    harness = Harness("call:\n call.l r4\nstub:\n ret.l", entry="stub")
    before = harness.snapshot()
    with pytest.raises(ValueError):
        harness.begin(arguments=())
    assert harness.snapshot() == before and harness.runner.last_segment_v3() is None
    result = harness.begin()
    assert result.exit_kind == "invalid_callback" and result.instructions == result.cycles == 0
    assert result.outputs == () and result.token is None
    _assert_root_receipt(harness, result, started=True, callbacks=0)
    assert result.segment_id == 1
    harness.state.set_reg(4, 99)  # A terminal profile failure released the reservation.
    harness.runner.close()


def test_completed_store_prefix_survives_rejected_grant_access_and_settles_exact_work():
    source = """
        ldi r6, 90
        st.b r4, r6
        addi r4, 8
    denied:
        str r4, r6
        ret.l
    """
    harness = Harness(source, inputs=1, outputs=0, callbacks=(), max_callback_requests=0)
    reference = OrdinaryReference(harness)
    reference.begin((EXT_BASE,))
    for _ in range(3):
        reference.step()
    result = harness.begin((EXT_BASE,), ((EXT_BASE, 1, "write"),))
    assert result.exit_kind == "rejected_access" and result.instructions == 3
    assert result.cycles == reference.cycles
    assert (result.access_address, result.access_width, result.access_operation) == (EXT_BASE + 8, 8, "write")
    assert result.instruction_pc == harness.labels["denied"] and result.outputs == ()
    assert bytes(harness.external) == bytes(reference.memory[:EXT_SIZE])
    assert harness.external[0] == 90 and bytes(harness.external[8:16]) == bytes(8)
    assert harness.state.get_reg(4) == EXT_BASE + 8 and harness.state.get_reg(6) == 90
    _assert_root_receipt(harness, result, started=True, callbacks=0)
    harness.runner.close()


@pytest.mark.parametrize("entry,target,expected", [("stub", "stub", 0), (0, "after", 1)])
def test_non_call_entry_and_wrong_target_never_create_callback_authority(entry, target, expected):
    harness = Harness("call:\n call.l r4\nafter:\n ret.l\nstub:\n ret.l", entry=entry)
    result = harness.begin((harness.labels[target], 0))
    assert result.exit_kind == "invalid_callback" and result.instructions == expected
    assert result.token is None and result.callback is None and result.outputs == ()
    if expected:
        assert int.from_bytes(harness.control[-16:-8], "little") == harness.labels["after"]
    _assert_root_receipt(harness, result, started=True, callbacks=0)
    harness.runner.close()


def test_failed_call_push_has_no_completed_instruction_or_callback_but_keeps_partial_sp():
    harness = Harness("call:\n call.l r4\nafter:\n ret.l\nstub:\n ret.l", stack_size=8)
    result = harness.begin((harness.labels["stub"], 0))
    assert result.exit_kind == "rejected_access" and result.instructions == result.cycles == 0
    assert result.access_address == CONTROL_BASE - 8
    assert harness.state.get_reg(15) == CONTROL_BASE - 8
    assert result.callback is None and result.token is None
    _assert_root_receipt(harness, result, started=True, callbacks=0)
    harness.runner.close()


@pytest.mark.parametrize("outputs", [(), (1, 2), (True,), (-1,), (1 << 64,), (1.0,)])
def test_malformed_reply_leaves_pending_authority_receipt_and_machine_state_usable(outputs):
    harness = Harness()
    first = harness.begin()
    _request(first)
    before, receipt = harness.snapshot(), _receipt(harness.runner.last_segment_v3())
    with pytest.raises((TypeError, ValueError)):
        harness.runner.resume_callback_v3(first.token, outputs)
    assert harness.snapshot() == before and _receipt(harness.runner.last_segment_v3()) == receipt
    assert harness.runner.resume_callback_v3(first.token, (3,)).outputs == (3,)
    harness.runner.close()


def test_opaque_foreign_replayed_and_previous_tokens_do_not_consume_current_request():
    first, second = Harness(), Harness()
    one, two = first.begin(), second.begin()
    for copier in (copy.copy, copy.deepcopy, pickle.dumps):
        with pytest.raises(TypeError):
            copier(one.token)
    with pytest.raises(TypeError):
        native.RoutineCallbackTokenV3()
    before = first.snapshot(), second.snapshot()
    for operation in (lambda: second.runner.resume_callback_v3(one.token, (3,)),
                      lambda: second.runner.cancel_chain_v3(one.token)):
        with pytest.raises((ValueError, RuntimeError)):
            operation()
    assert (first.snapshot(), second.snapshot()) == before
    first.runner.resume_callback_v3(one.token, (3,))
    current = first.begin()
    before = first.snapshot()
    with pytest.raises((ValueError, RuntimeError)):
        first.runner.resume_callback_v3(one.token, (99,))
    assert first.snapshot() == before
    assert first.runner.resume_callback_v3(current.token, (3,)).outputs == (3,)
    assert second.runner.resume_callback_v3(two.token, (3,)).outputs == (3,)
    first.runner.close()
    second.runner.close()


@pytest.mark.parametrize("mutation", ["code", "return_cell"])
def test_changed_parked_evidence_rejects_outputs_but_owner_cancel_preserves_diagnostics(mutation):
    harness = Harness()
    first = harness.begin()
    if mutation == "code":
        harness.ram[harness.labels["stub"]] = 0x01
    else:
        slot = harness.state.get_reg(15) - CONTROL_BASE
        harness.control[slot:slot + 8] = MASK64.to_bytes(8, "little")
    before, receipt = harness.snapshot(), _receipt(harness.runner.last_segment_v3())
    with pytest.raises((ValueError, RuntimeError)):
        harness.runner.resume_callback_v3(first.token, (99,))
    assert harness.snapshot() == before and _receipt(harness.runner.last_segment_v3()) == receipt
    cancelled = harness.runner.cancel_chain_v3()
    assert cancelled.exit_kind == "cancelled" and cancelled.instructions == cancelled.cycles == 0
    assert harness.snapshot() == before and _receipt(harness.runner.last_segment_v3()) == receipt
    assert harness.runner.cancel_chain_v3() is None
    harness.runner.close()


def test_every_legacy_route_and_public_cpu_mutation_rejects_parked_v3_chain():
    harness = Harness()
    harness.legacy.publish_code_v2(harness.v2)
    first = harness.begin()
    before = harness.snapshot()
    for operation in (
        lambda: harness.legacy.run(harness.v1, (99, 99), (), 100),
        lambda: harness.legacy.begin_v2(harness.v2, (99, 99), (), 100),
        lambda: harness.legacy.publish_code(harness.v1),
        lambda: harness.legacy.publish_code_v2(harness.v2),
        lambda: harness.legacy.revoke_code_v2(harness.v2),
        lambda: harness.legacy.is_code_published_v2(harness.v2),
        harness.legacy.cancel_invocation, harness.legacy.close,
        lambda: native.RoutineRunnerV1.close(harness.legacy),
        lambda: harness.state.set_reg(4, 99), lambda: harness.state.flags_unpack(255),
        lambda: harness.state.icache_reset(),
        lambda: harness.begin(), lambda: harness.runner.publish_code_v3(harness.spec),
        lambda: harness.runner.revoke_code_v3(harness.spec),
    ):
        with pytest.raises(RuntimeError):
            operation()
        assert harness.snapshot() == before
    assert harness.legacy.last_segment_v2() is None
    assert harness.runner.is_code_published_v3(harness.spec)
    assert harness.snapshot() == before
    assert harness.runner.last_segment_v3().segment_id == first.segment_id
    assert harness.runner.resume_callback_v3(first.token, (3,)).outputs == (3,)
    harness.runner.close()


def test_v2_v3_v2_work_keeps_each_transport_receipt_sequence_and_invocation_space_separate():
    harness = Harness()
    harness.legacy.publish_code_v2(harness.v2)
    old_first = harness.legacy.begin_v2(harness.v2, (7, 3), (), 100)
    old_final = harness.legacy.resume_callback(old_first.token, (3,))
    old_receipt = harness.legacy.last_segment_v2()
    assert (old_first.segment_id, old_final.segment_id, old_first.invocation_id) == (1, 2, 1)
    assert harness.runner.last_segment_v3() is None
    new_first = harness.begin()
    new_final = harness.runner.resume_callback_v3(new_first.token, (3,))
    assert (new_first.segment_id, new_final.segment_id, new_first.invocation_id) == (1, 2, 1)
    assert harness.legacy.last_segment_v2().segment_id == old_receipt.segment_id
    again = harness.legacy.begin_v2(harness.v2, (7, 3), (), 100)
    assert (again.segment_id, again.invocation_id) == (3, 2)
    assert harness.runner.last_segment_v3().segment_id == new_final.segment_id
    harness.legacy.cancel_invocation()
    harness.runner.close()


@pytest.mark.parametrize("phase", ["begin", "resume"])
def test_lost_host_result_still_exposes_receipt_for_no_token_cleanup(phase):
    """Delivery-loss proxy; actual pybind allocation-failure cleanup is separately reviewed."""
    harness = Harness(LOOP, max_callback_requests=2)
    error = RuntimeError("host discarded the delivered native result")

    def lose_result(operation, *arguments):
        operation(*arguments)
        raise error

    with pytest.raises(RuntimeError) as caught:
        if phase == "begin":
            lose_result(harness.begin)
        else:
            first = harness.begin()
            lose_result(harness.runner.resume_callback_v3, first.token, (3,))
    assert caught.value is error
    receipt = harness.runner.last_segment_v3()
    assert receipt.callback_request is True
    assert (receipt.segment_id, receipt.instructions, receipt.invocation_instructions,
            receipt.invocation_callbacks) == ((1, 3, 3, 1) if phase == "begin" else (2, 4, 7, 2))
    before = harness.snapshot()
    harness.runner.cancel_chain_v3()
    assert harness.snapshot() == before and _receipt(harness.runner.last_segment_v3()) == _receipt(receipt)
    assert harness.runner.cancel_chain_v3() is None
    result = harness.legacy.run(harness.v1, (7, 3), (), 100)
    assert result.exit_kind == "returned" and result.outputs == (7,)
    assert harness.state.cycle_count > before[5]
    harness.runner.close()


def test_close_cancels_parked_root_preserves_receipt_and_releases_pin_despite_retained_token():
    harness = Harness()
    first = harness.begin()
    receipt = _receipt(harness.runner.last_segment_v3())
    before = harness.snapshot()
    harness.runner.close()
    assert harness.snapshot() == before
    assert _receipt(harness.runner.last_segment_v3()) == receipt
    with pytest.raises(RuntimeError):
        harness.runner.resume_callback_v3(first.token, (3,))
    assert harness.runner.cancel_chain_v3() is None
    harness.control.extend(b"x")
    harness.state.attach_mem(bytearray(RAM_SIZE), RAM_SIZE)


@pytest.mark.parametrize("accepted_results", [1, 2])
def test_real_marshalling_failure_cancels_chain_retains_prefix_receipt_and_is_one_shot(accepted_results):
    harness = Harness(LOOP, max_callback_requests=2)
    harness.runner._test_fail_marshalling_after_results(accepted_results)
    before = harness.snapshot()
    with pytest.raises(ValueError):
        harness.begin(arguments=())
    assert harness.snapshot() == before and harness.runner.last_segment_v3() is None
    if accepted_results == 1:
        with pytest.raises(MemoryError):
            harness.begin()
    else:
        first = harness.begin()
        _request(first)
        before = harness.snapshot()
        with pytest.raises(TypeError):
            harness.runner.resume_callback_v3(first.token, (True,))
        assert harness.snapshot() == before
        with pytest.raises(MemoryError):
            harness.runner.resume_callback_v3(first.token, (3,))
    receipt = harness.runner.last_segment_v3()
    assert receipt.callback_request is True
    assert (receipt.segment_id, receipt.instructions, receipt.invocation_instructions,
            receipt.invocation_callbacks) == ((1, 3, 3, 1) if accepted_results == 1 else (2, 4, 7, 2))
    assert receipt.invocation_started is (accepted_results == 1)
    assert receipt.chain_instructions == receipt.invocation_instructions
    assert receipt.chain_cycles == receipt.invocation_cycles
    before = harness.snapshot()
    # The binding's actual marshalling catch already revoked the parked chain.
    assert harness.runner.cancel_chain_v3() is None
    assert harness.snapshot() == before and _receipt(harness.runner.last_segment_v3()) == _receipt(receipt)
    harness.state.set_reg(4, 99)
    again = harness.begin()
    _request(again)
    assert again.invocation_id > receipt.invocation_id
    assert again.segment_id == receipt.segment_id + 1
    harness.runner.cancel_chain_v3()
    harness.runner.close()


def test_real_marshalling_failure_after_zero_work_resume_preserves_unapplied_reply_and_receipt():
    harness = Harness()
    harness.runner._test_fail_marshalling_after_results(2)
    first = harness.begin(instructions=3)
    before = harness.snapshot()
    with pytest.raises(MemoryError):
        harness.runner.resume_callback_v3(first.token, (99,))
    assert harness.snapshot() == before
    receipt = harness.runner.last_segment_v3()
    assert receipt.segment_id == first.segment_id + 1
    assert receipt.instructions == receipt.cycles == 0
    assert receipt.invocation_instructions == receipt.chain_instructions == 3
    assert receipt.invocation_callbacks == receipt.chain_callbacks == 1
    assert receipt.callback_request is False and receipt.invocation_started is False
    assert harness.runner.cancel_chain_v3() is None
    with pytest.raises((ValueError, RuntimeError)):
        harness.runner.resume_callback_v3(first.token, (3,))
    harness.runner.close()


@pytest.mark.parametrize("count", [0, 65, -1, True, False, 1.0, "1", None])
def test_private_marshalling_failpoint_is_strict_and_invalid_arming_has_no_effect(count):
    harness = Harness()
    before = harness.snapshot()
    with pytest.raises((TypeError, ValueError)):
        harness.runner._test_fail_marshalling_after_results(count)
    assert harness.snapshot() == before and harness.runner.last_segment_v3() is None
    first = harness.begin()
    _request(first)
    harness.runner.cancel_chain_v3()
    harness.runner.close()


def test_private_failpoint_cannot_be_armed_or_reset_while_either_transport_is_parked():
    harness = Harness()
    harness.legacy.publish_code_v2(harness.v2)
    old = harness.legacy.begin_v2(harness.v2, (7, 3), (), 100)
    before = harness.snapshot()
    with pytest.raises(RuntimeError):
        harness.runner._test_fail_marshalling_after_results(1)
    assert harness.snapshot() == before
    harness.legacy.resume_callback(old.token, (3,))
    harness.runner._test_fail_marshalling_after_results(2)
    first = harness.begin()
    before = harness.snapshot()
    with pytest.raises(RuntimeError):
        harness.runner._test_fail_marshalling_after_results(1)
    assert harness.snapshot() == before
    with pytest.raises(MemoryError):
        harness.runner.resume_callback_v3(first.token, (3,))
    assert harness.runner.cancel_chain_v3() is None
    harness.runner.close()
