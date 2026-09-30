"""Native children preserve real instruction effects and their parked parent."""

from __future__ import annotations

import _mp64_accel as native
import pytest

from asm import assemble


MASK64 = (1 << 64) - 1
RAM_SIZE, EXT_BASE, EXT_SIZE = 65536, 0x100000, 4096
CONTROL_BASE, STACK_SIZE, CONTROL_SIZE = EXT_BASE + EXT_SIZE, 128, 1280
MMIO_BASE, MMIO_END = 0xFFFFFF0000000000, 0xFFFFFF8000000000
CONTROL_FIELDS = (
    "psel", "xsel", "spsel", "sw", "d_reg", "q_out", "t_reg", "ef_flags",
    "halted", "idle", "ext_modifier", "ivt_base", "ivec_id", "trap_addr", "wake_ms",
    "priv_level", "core_id", "num_cores", "irq_ipi", "icache_enabled",
)
CONTROL_VALUES = (3, 2, 15, 1, 0, 0, 0, 0, False, False, -1, 0, 0, 0, 0, 0, 0, 1, False, 1)
RECEIPT_FIELDS = (
    "segment_id", "root_invocation_id", "invocation_id", "parent_invocation_id", "depth",
    "invocation_started", "instructions", "cycles", "invocation_instructions", "invocation_cycles",
    "invocation_callbacks", "chain_instructions", "chain_cycles", "chain_callbacks", "callback_request",
)
CALL = """
    ldi64 r12, stub
call:
    call.l r12
after:
    ret.l
stub:
    ret.l
"""
PARENT_BUFFER = """
    mov r13, r4
    mov r4, r5
    mov r5, r6
    ldi64 r10, 0xAABBCCDDEEFF0011
    ldi r16, 37
    ldi r31, 53
    ldi64 r12, stub
    cmp r4, r5
call:
    call.l r12
after:
    ldn r6, r13
    add r4, r6
    ret.l
stub:
    ret.l
"""
CHILD_BUFFER = """
    ldn r6, r4
    inc r6
    str r4, r6
    mov r4, r6
    ldi64 r10, 0x0102030405060708
    ldi r16, 71
    ldi r31, 89
    cmp r4, r5
    ret.l
"""


def _view(state):
    return (tuple(state.get_reg(index) for index in range(32)), state.flags_pack(),
            tuple(getattr(state, field) for field in CONTROL_FIELDS))


def _receipt(runner):
    value = runner.last_segment_v3()
    return None if value is None else tuple(getattr(value, name) for name in RECEIPT_FIELDS)


class Owner:
    def __init__(self):
        self.ram, self.external = bytearray(RAM_SIZE), bytearray(EXT_SIZE)
        self.control = bytearray(b"\xA5" * CONTROL_SIZE)
        self.state = native.CPUState()
        self.state.attach_mem(self.ram, RAM_SIZE)
        self.state.attach_ext_mem(self.external, EXT_BASE, EXT_SIZE)
        self.state.icache_control_write(1)
        self.runner = native.RoutineRunnerV3(self.state, CONTROL_BASE, self.control)
        self.legacy = self.runner.legacy_v2()
        self.labels = {}

    def spec(self, slot, source=CALL, *, inputs=1, outputs=1, callbacks=(("call", "stub", 1, 1, 1),),
             instructions=100, callback_limit=1, stack_size=STACK_SIZE):
        base, labels = 0x100 + slot * 0x400, {}
        raw = bytes(assemble(source, base_addr=base, labels_out=labels))
        code = raw + b"\x01" * (-len(raw) % 16)
        assert len(code) <= 0x400
        self.ram[base:base + len(code)] = code
        spec = native.RoutineSpecV3(code_base=base, code=code, entry_offset=0,
            input_cells=inputs, output_cells=outputs, stack_base=CONTROL_BASE + slot * STACK_SIZE,
            stack_size=stack_size, max_instructions=instructions, max_callback_requests=callback_limit,
            callbacks=tuple((labels[call] - base, labels[stub] - base, export, count_in, count_out)
                            for call, stub, export, count_in, count_out in callbacks))
        self.labels[id(spec)] = labels
        return spec

    def publish_pair(self, child_source="ret.l", *, child_inputs=1, child_outputs=1,
                     child_callbacks=(), child_limit=0, child_instructions=100):
        child = self.spec(1, child_source, inputs=child_inputs, outputs=child_outputs,
                          callbacks=child_callbacks, callback_limit=child_limit,
                          instructions=child_instructions)
        parent = self.spec(0)
        self.runner.publish_code_v3(child)
        edge, = self.runner.publish_code_v3(parent, ((0, 0, child),))
        return parent, child, edge

    def begin(self, spec, arguments=(7,), spans=(), *, instructions=1000, callbacks=1024, protected=()):
        return self.runner.begin_root_v3(spec, arguments, spans, instructions,
                                        callback_limit=callbacks, protected_spans=protected)

    def snapshot(self):
        return (_view(self.state), self.state.cycle_count, self.state.icache_hits, self.state.icache_misses,
                self.state.icache_snapshot(), bytes(self.ram), bytes(self.external), bytes(self.control))


class OrdinaryReference:
    """Ordinary step_one execution, with explicit host context switching only."""

    def __init__(self, owner):
        self.owner = owner
        self.ram, self.memory = bytearray(owner.ram), bytearray(owner.external + owner.control)
        self.state = native.CPUState()
        self.state.attach_mem(self.ram, RAM_SIZE)
        self.state.attach_ext_mem(self.memory, EXT_BASE, len(self.memory))
        self.state.icache_control_write(1)
        self.instructions = self.cycles = 0

    def enter(self, spec, arguments):
        for index in range(32):
            self.state.set_reg(index, 0)
        for field, value in zip(CONTROL_FIELDS, CONTROL_VALUES):
            setattr(self.state, field, value)
        self.state.flags_unpack(0)
        self.state.set_reg(3, spec.code_base + spec.entry_offset)
        top = spec.stack_base + spec.stack_size
        self.state.set_reg(15, top - 8)
        self.memory[top - 8 - EXT_BASE:top - EXT_BASE] = MASK64.to_bytes(8, "little")
        self.reply(arguments)

    def reply(self, values):
        for index, value in enumerate(values):
            self.state.set_reg(4 + index, value)

    def step(self):
        def unexpected(*_args):
            pytest.fail("ordinary integer reference reached a device")

        cycles = native.step_one(self.state, mmio_read8=unexpected, mmio_write8=unexpected,
            on_output=unexpected, csr_read_override=None, mmio_start=MMIO_BASE, mmio_end=MMIO_END)
        self.instructions += 1
        self.cycles += cycles

    def segment(self, spec):
        before = self.instructions, self.cycles
        stops = {spec.code_base + site[1] for site in spec.callbacks} | {MASK64}
        while True:
            assert self.instructions - before[0] < 100, "ordinary reference failed to reach a boundary"
            self.step()
            if self.state.get_reg(3) in stops:
                return self.instructions - before[0], self.cycles - before[1]

    def restore(self, view):
        registers, flags, controls = view
        for index, value in enumerate(registers):
            self.state.set_reg(index, value)
        self.state.flags_unpack(flags)
        for field, value in zip(CONTROL_FIELDS, controls):
            setattr(self.state, field, value)

    def assert_equal(self):
        owner = self.owner
        assert _view(owner.state) == _view(self.state)
        assert bytes(owner.ram) == bytes(self.ram)
        assert bytes(owner.external) == bytes(self.memory[:EXT_SIZE])
        assert bytes(owner.control) == bytes(self.memory[EXT_SIZE:])
        assert owner.state.cycle_count == self.state.cycle_count
        assert (owner.state.icache_hits, owner.state.icache_misses, owner.state.icache_snapshot()) == (
            self.state.icache_hits, self.state.icache_misses, self.state.icache_snapshot())


def _assert_receipt(owner, result):
    assert _receipt(owner.runner) == tuple(result.exit_kind == "callback_request"
        if name == "callback_request" else getattr(result, name) for name in RECEIPT_FIELDS)


def test_real_child_return_restores_parent_only_and_matches_ordinary_cold_and_warm_execution():
    owner = Owner()
    child = owner.spec(1, CHILD_BUFFER, inputs=2, callbacks=(), callback_limit=0)
    parent = owner.spec(0, PARENT_BUFFER, inputs=3, callbacks=(("call", "stub", 7, 2, 1),))
    owner.runner.publish_code_v3(child)
    edge, = owner.runner.publish_code_v3(parent, ((0, 17, child),))
    reference = OrdinaryReference(owner)
    previous_segment = 0
    for _ in range(2):
        owner.external[:8] = (41).to_bytes(8, "little")
        reference.memory[:8] = (41).to_bytes(8, "little")
        reference.enter(parent, (EXT_BASE, 7, 3))
        first = owner.begin(parent, (EXT_BASE, 7, 3), ((EXT_BASE, 16, "read_write"),))
        assert first.exit_kind == "callback_request" and first.callback.arguments == (7, 3)
        assert (first.instructions, first.cycles) == reference.segment(parent)
        assert first.instructions == 9 and first.segment_id == previous_segment + 1
        reference.assert_equal()
        parent_view, parent_bytes = _view(owner.state), bytes(owner.control[:STACK_SIZE])
        cycles_before, reference_parent = owner.state.cycle_count, _view(reference.state)

        reference.enter(child, (EXT_BASE, 999))
        completed = owner.runner.begin_child_v3(first.token, edge, (EXT_BASE, 999),
                                                ((EXT_BASE, 8, "read_write"),))
        assert completed.exit_kind == "returned" and completed.outputs == (42,)
        assert (completed.instructions, completed.cycles) == reference.segment(child)
        assert completed.instructions == completed.invocation_instructions == 9
        assert (completed.pc, completed.parent_invocation_id, completed.depth) == (MASK64, first.invocation_id, 2)
        assert completed.root_invocation_id == first.invocation_id and completed.invocation_id > first.invocation_id
        assert completed.invocation_started is True and completed.invocation_callbacks == 0
        assert completed.chain_instructions == 18 and completed.chain_callbacks == 1
        assert completed.token is None and completed.callback is None
        assert _view(owner.state) == parent_view
        assert bytes(owner.control[:STACK_SIZE]) == parent_bytes
        assert owner.state.cycle_count == cycles_before + completed.cycles
        reference.restore(reference_parent)
        reference.assert_equal()
        _assert_receipt(owner, completed)

        reference.reply(completed.outputs)
        final = owner.runner.resume_callback_v3(first.token, completed.outputs)
        assert final.exit_kind == "returned" and final.outputs == (84,)
        assert (final.instructions, final.cycles) == reference.segment(parent)
        assert final.instructions == 4 and final.invocation_instructions == 13
        assert final.chain_instructions == 22 and final.chain_callbacks == 1
        assert final.chain_cycles == first.cycles + completed.cycles + final.cycles
        assert final.invocation_cycles == first.cycles + final.cycles
        assert final.depth == 1 and final.invocation_id == first.invocation_id
        assert final.invocation_started is False
        reference.assert_equal()
        _assert_receipt(owner, final)
        previous_segment = final.segment_id
    assert type(native.HYBRID_NESTED_ROUTINE_ABI_VERSION) is int
    assert native.HYBRID_NESTED_ROUTINE_ABI_VERSION == 3
    assert type(native.HYBRID_NESTED_ROUTINE_CAPABILITY) is str
    assert native.HYBRID_NESTED_ROUTINE_CAPABILITY == "distinct_registration_children"
    assert type(native.HYBRID_NESTED_ROUTINE_MAX_DEPTH) is int
    assert native.HYBRID_NESTED_ROUTINE_MAX_DEPTH == 8
    owner.runner.close()


def test_sequential_child_reuse_keeps_the_same_parent_request_and_distinct_child_receipts():
    owner = Owner()
    parent, child, edge = owner.publish_pair("inc r4\nret.l")
    first = owner.begin(parent)
    parent_view = _view(owner.state)
    prior_id, prior_segment = first.invocation_id, first.segment_id
    for value in (10, 20, 30):
        completed = owner.runner.begin_child_v3(first.token, edge, (value,), ())
        assert completed.outputs == (value + 1,) and completed.instructions == 2
        assert completed.invocation_id > prior_id and completed.segment_id == prior_segment + 1
        assert completed.parent_invocation_id == first.invocation_id and completed.depth == 2
        assert completed.invocation_started and completed.invocation_instructions == 2
        assert _view(owner.state) == parent_view
        prior_id, prior_segment = completed.invocation_id, completed.segment_id
    final = owner.runner.resume_callback_v3(first.token, (31,))
    assert final.outputs == (31,) and final.invocation_instructions == 4 and final.chain_instructions == 10
    assert final.invocation_callbacks == final.chain_callbacks == 1
    with pytest.raises((ValueError, RuntimeError)):
        owner.runner.begin_child_v3(first.token, edge, (1,), ())
    owner.runner.close()


def test_eight_distinct_frames_unwind_without_ancestor_work_or_token_replay():
    owner = Owner()
    specs = [owner.spec(index) for index in range(8)]
    edges = [None] * 7
    owner.runner.publish_code_v3(specs[-1])
    for index in reversed(range(7)):
        edges[index], = owner.runner.publish_code_v3(specs[index], ((0, index, specs[index + 1]),))
    requests = [owner.begin(specs[0], (0,))]
    views = [_view(owner.state)]
    for index in range(7):
        result = owner.runner.begin_child_v3(requests[-1].token, edges[index], (index + 1,), ())
        assert result.exit_kind == "callback_request" and result.depth == index + 2
        assert result.instructions == result.invocation_instructions == 2
        assert result.invocation_started and result.parent_invocation_id == requests[-1].invocation_id
        assert result.root_invocation_id == requests[0].invocation_id
        assert result.chain_instructions == 2 * (index + 2) and result.chain_callbacks == index + 2
        requests.append(result)
        views.append(_view(owner.state))
    before, receipt = owner.snapshot(), _receipt(owner.runner)
    for request in requests[:-1]:
        with pytest.raises((ValueError, RuntimeError)):
            owner.runner.resume_callback_v3(request.token, (99,))
        with pytest.raises((ValueError, RuntimeError)):
            owner.runner.cancel_chain_v3(request.token)
        assert owner.snapshot() == before and _receipt(owner.runner) == receipt
    for spec in specs:
        assert owner.runner.is_code_published_v3(spec)
        assert owner.snapshot() == before and _receipt(owner.runner) == receipt
    for index in reversed(range(8)):
        result = owner.runner.resume_callback_v3(requests[index].token, (8 - index,))
        assert result.exit_kind == "returned" and result.outputs == (8 - index,)
        assert result.depth == index + 1 and result.invocation_id == requests[index].invocation_id
        assert result.instructions == 2 and result.invocation_instructions == 4
        assert result.invocation_callbacks == 1 and result.chain_callbacks == 8
        assert result.chain_instructions == 16 + 2 * (8 - index)
        assert not result.invocation_started
        if index:
            assert _view(owner.state) == views[index - 1]
        _assert_receipt(owner, result)
    assert result.chain_instructions == 32 and result.outputs == (8,)
    assert owner.runner.cancel_chain_v3() is None
    owner.runner.close()


@pytest.mark.parametrize("fault", ["missing_token", "foreign_token", "missing_edge", "foreign_edge",
    "stale_edge", "wrong_site", "arity", "boolean_cell", "oversized_cell"])
def test_child_authority_and_argument_preflight_preserve_parent_and_allow_exact_retry(fault):
    owner, foreign = Owner(), Owner()
    child = owner.spec(1, "ret.l", callbacks=(), callback_limit=0)
    source = """
        ldi64 r12, stub0
    call0:
        call.l r12
        ldi64 r12, stub1
    call1:
        call.l r12
        ret.l
    stub0:
        ret.l
    stub1:
        ret.l
    """
    parent = owner.spec(0, source, callbacks=(("call0", "stub0", 1, 1, 1),
                                            ("call1", "stub1", 2, 1, 1)), callback_limit=2)
    owner.runner.publish_code_v3(child)
    stale, _ = owner.runner.publish_code_v3(parent, ((0, 0, child), (1, 0, child)))
    owner.runner.revoke_code_v3(parent)
    edge, wrong_site = owner.runner.publish_code_v3(parent, ((0, 0, child), (1, 0, child)))
    foreign_parent, _, foreign_edge = foreign.publish_pair()
    other = foreign.begin(foreign_parent)
    first = owner.begin(parent)
    token, chosen, arguments = first.token, edge, (9,)
    if fault == "missing_token":
        token = None
    elif fault == "foreign_token":
        token = other.token
    elif fault == "missing_edge":
        chosen = None
    elif fault == "foreign_edge":
        chosen = foreign_edge
    elif fault == "stale_edge":
        chosen = stale
    elif fault == "wrong_site":
        chosen = wrong_site
    elif fault == "arity":
        arguments = ()
    elif fault == "boolean_cell":
        arguments = (True,)
    else:
        arguments = (MASK64 + 1,)
    before, receipt = owner.snapshot(), _receipt(owner.runner)
    with pytest.raises((TypeError, ValueError, RuntimeError)):
        owner.runner.begin_child_v3(token, chosen, arguments, ())
    assert owner.snapshot() == before and _receipt(owner.runner) == receipt
    completed = owner.runner.begin_child_v3(first.token, edge, (9,), ())
    assert completed.outputs == (9,) and completed.segment_id == first.segment_id + 1
    assert completed.invocation_id == first.invocation_id + 1
    second = owner.runner.resume_callback_v3(first.token, completed.outputs)
    assert second.exit_kind == "callback_request" and second.callback.site_index == 1
    assert owner.runner.resume_callback_v3(second.token, (9,)).outputs == (9,)
    foreign.runner.cancel_chain_v3()
    foreign.runner.close()
    owner.runner.close()


@pytest.mark.parametrize("parent_spans,child_spans,protected", [
    ((), ((EXT_BASE, 1, "read"),), ()),
    (((EXT_BASE, 8, "read"),), ((EXT_BASE, 8, "write"),), ()),
    (((EXT_BASE, 8, "write"),), ((EXT_BASE, 8, "read"),), ()),
    (((EXT_BASE, 8, "read_write"),), ((EXT_BASE + 7, 2, "read"),), ()),
    (((EXT_BASE, 8, "read"), (EXT_BASE + 8, 8, "read")), ((EXT_BASE + 4, 8, "read"),), ()),
    (((EXT_BASE, 16, "read_write"),), ((EXT_BASE + 4, 8, "write"),), ((EXT_BASE + 6, 1),)),
    (((EXT_BASE, 16, "read_write"),), ((CONTROL_BASE, 1, "write"),), ()),
    (((EXT_BASE, 16, "read_write"),), ((0x100, 1, "read"),), ()),
    (((EXT_BASE, 16, "read_write"),), ((MMIO_BASE, 1, "read"),), ()),
    (((EXT_BASE, 16, "read_write"),), ((MASK64 - 3, 8, "read"),), ()),
])
def test_child_grants_must_fit_one_immediate_parent_grant_and_all_protected_spans(
        parent_spans, child_spans, protected):
    owner = Owner()
    parent, _, edge = owner.publish_pair()
    first = owner.begin(parent, spans=parent_spans)
    before, receipt = owner.snapshot(), _receipt(owner.runner)
    with pytest.raises((TypeError, ValueError)):
        owner.runner.begin_child_v3(first.token, edge, (9,), child_spans, protected_spans=protected)
    assert owner.snapshot() == before and _receipt(owner.runner) == receipt
    completed = owner.runner.begin_child_v3(first.token, edge, (9,), ())
    assert completed.outputs == (9,) and completed.invocation_id == first.invocation_id + 1
    assert owner.runner.resume_callback_v3(first.token, completed.outputs).outputs == (9,)
    owner.runner.close()


@pytest.mark.parametrize("access", ["read", "write", "read_write"])
def test_narrowed_child_grants_and_empty_grants_are_admitted(access):
    owner = Owner()
    parent, _, edge = owner.publish_pair()
    first = owner.begin(parent, spans=((EXT_BASE, 16, "read_write"),))
    child = owner.runner.begin_child_v3(first.token, edge, (9,),
                                      ((EXT_BASE + 4, 4, access), (EXT_BASE + 32, 0, "read_write")))
    assert child.outputs == (9,) and child.instructions == 1
    assert owner.runner.resume_callback_v3(first.token, child.outputs).outputs == (9,)
    owner.runner.close()


def test_descendant_cannot_reborrow_a_root_range_omitted_from_its_immediate_parent():
    owner = Owner()
    leaf, child, parent = owner.spec(2, "ret.l", callbacks=(), callback_limit=0), owner.spec(1), owner.spec(0)
    owner.runner.publish_code_v3(leaf)
    leaf_edge, = owner.runner.publish_code_v3(child, ((0, 0, leaf),))
    child_edge, = owner.runner.publish_code_v3(parent, ((0, 0, child),))
    root = owner.begin(parent, spans=((EXT_BASE, 32, "read_write"),))
    middle = owner.runner.begin_child_v3(root.token, child_edge, (1,), ((EXT_BASE, 8, "read"),))
    before, receipt = owner.snapshot(), _receipt(owner.runner)
    with pytest.raises(ValueError):
        owner.runner.begin_child_v3(middle.token, leaf_edge, (2,), ((EXT_BASE + 16, 8, "read"),))
    assert owner.snapshot() == before and _receipt(owner.runner) == receipt
    last = owner.runner.begin_child_v3(middle.token, leaf_edge, (2,), ((EXT_BASE, 8, "read"),))
    assert last.depth == 3 and last.outputs == (2,)
    child_done = owner.runner.resume_callback_v3(middle.token, last.outputs)
    assert owner.runner.resume_callback_v3(root.token, child_done.outputs).outputs == (2,)
    owner.runner.close()


def test_exhausted_instruction_remainder_rejects_child_before_entry_and_preserves_zero_work_parent_exit():
    owner = Owner()
    parent, _, edge = owner.publish_pair()
    first = owner.begin(parent, instructions=2)
    assert first.exit_kind == "callback_request" and first.instructions == 2
    before, receipt = owner.snapshot(), _receipt(owner.runner)
    with pytest.raises(ValueError):
        owner.runner.begin_child_v3(first.token, edge, (9,), ())
    assert owner.snapshot() == before and _receipt(owner.runner) == receipt
    final = owner.runner.resume_callback_v3(first.token, (99,))
    assert final.exit_kind == "instruction_limit" and final.instructions == final.cycles == 0
    assert final.invocation_id == first.invocation_id and final.chain_instructions == 2
    assert owner.snapshot() == before
    _assert_receipt(owner, final)
    owner.runner.close()


@pytest.mark.parametrize("local,root,expected", [(1, 100, "instruction_limit"),
                                               (100, 3, "instruction_limit"),
                                               (2, 100, "returned")])
def test_child_uses_its_own_limit_and_the_unrenewed_root_instruction_allowance(local, root, expected):
    owner = Owner()
    parent, _, edge = owner.publish_pair("inc r4\nret.l", child_instructions=local)
    first = owner.begin(parent, instructions=root)
    child = owner.runner.begin_child_v3(first.token, edge, (10,), ())
    assert child.exit_kind == expected and child.depth == 2
    assert child.instructions == child.invocation_instructions == (2 if expected == "returned" else 1)
    assert child.chain_instructions == first.instructions + child.instructions
    _assert_receipt(owner, child)
    if expected == "returned":
        assert child.outputs == (11,)
        assert owner.runner.resume_callback_v3(first.token, child.outputs).outputs == (11,)
    else:
        assert child.outputs == () and owner.state.get_reg(4) == 11
        assert owner.state.get_reg(3) != first.pc
        assert owner.runner.cancel_chain_v3() is None
        with pytest.raises((ValueError, RuntimeError)):
            owner.runner.resume_callback_v3(first.token, (99,))
    owner.runner.close()


def test_callback_free_child_can_run_after_root_callback_allowance_is_consumed():
    owner = Owner()
    parent, _, edge = owner.publish_pair()
    first = owner.begin(parent, callbacks=1)
    child = owner.runner.begin_child_v3(first.token, edge, (9,), ())
    assert child.outputs == (9,) and child.chain_callbacks == 1 and child.invocation_callbacks == 0
    assert owner.runner.resume_callback_v3(first.token, child.outputs).outputs == (9,)
    owner.runner.close()


@pytest.mark.parametrize("child_limit,root_limit", [(0, 1024), (1, 1)])
def test_child_callback_ceilings_retain_completed_call_effects_without_issuing_authority(child_limit, root_limit):
    owner = Owner()
    parent, child_spec, edge = owner.publish_pair(CALL, child_callbacks=(("call", "stub", 2, 1, 1),),
                                                child_limit=child_limit)
    first = owner.begin(parent, callbacks=root_limit)
    child = owner.runner.begin_child_v3(first.token, edge, (9,), ())
    assert child.exit_kind == "callback_limit" and child.instructions == 2
    assert child.invocation_callbacks == 0 and child.chain_callbacks == 1
    assert child.token is None and child.callback is None and child.outputs == ()
    assert owner.state.get_reg(3) == owner.labels[id(child_spec)]["stub"]
    offset = owner.state.get_reg(15) - CONTROL_BASE
    assert int.from_bytes(owner.control[offset:offset + 8], "little") == owner.labels[id(child_spec)]["after"]
    assert owner.runner.cancel_chain_v3() is None
    _assert_receipt(owner, child)
    owner.runner.close()


def test_failed_child_store_prefix_matches_ordinary_work_without_restoring_parent_or_outputs():
    owner = Owner()
    source = """
        ldi r6, 90
        st.b r4, r6
        addi r4, 8
    denied:
        str r4, r6
        ret.l
    """
    parent, child_spec, edge = owner.publish_pair(source, child_outputs=0)
    reference = OrdinaryReference(owner)
    reference.enter(parent, (7,))
    first = owner.begin(parent, spans=((EXT_BASE, 16, "write"),))
    assert (first.instructions, first.cycles) == reference.segment(parent)
    parent_view, parent_control = _view(owner.state), bytes(owner.control[:STACK_SIZE])
    reference.enter(child_spec, (EXT_BASE,))
    before_steps, before_cycles = reference.instructions, reference.cycles
    for _ in range(3):
        reference.step()
    failed = owner.runner.begin_child_v3(first.token, edge, (EXT_BASE,), ((EXT_BASE, 1, "write"),))
    assert failed.exit_kind == "rejected_access" and failed.outputs == () and failed.token is None
    assert (failed.instructions, failed.cycles) == (
        reference.instructions - before_steps, reference.cycles - before_cycles)
    assert failed.instructions == failed.invocation_instructions == 3 and failed.chain_instructions == 5
    assert failed.invocation_started and failed.depth == 2 and failed.parent_invocation_id == first.invocation_id
    assert (failed.access_address, failed.access_width, failed.access_operation) == (EXT_BASE + 8, 8, "write")
    assert failed.instruction_pc == owner.labels[id(child_spec)]["denied"]
    assert bytes(owner.external) == bytes(reference.memory[:EXT_SIZE])
    assert owner.external[0] == 90 and bytes(owner.external[8:16]) == bytes(8)
    assert bytes(owner.control) == bytes(reference.memory[EXT_SIZE:])
    assert bytes(owner.control[:STACK_SIZE]) == parent_control
    assert owner.state.get_reg(4) == EXT_BASE + 8 and owner.state.get_reg(6) == 90
    assert _view(owner.state) != parent_view
    assert owner.state.cycle_count == reference.state.cycle_count
    _assert_receipt(owner, failed)
    assert owner.runner.cancel_chain_v3() is None
    with pytest.raises((ValueError, RuntimeError)):
        owner.runner.resume_callback_v3(first.token, (99,))
    owner.state.set_reg(4, 17)  # Failure released the entire chain reservation.
    owner.runner.close()


@pytest.mark.parametrize("mutation", ["ancestor_code", "ancestor_control", "child_code"])
def test_ancestor_and_child_evidence_are_rechecked_before_reply_effects_and_exact_retry(mutation):
    owner = Owner()
    parent, child_spec, edge = owner.publish_pair(CALL, child_callbacks=(("call", "stub", 2, 1, 1),),
                                                child_limit=1)
    first = owner.begin(parent)
    parent_sp = owner.state.get_reg(15)
    child = owner.runner.begin_child_v3(first.token, edge, (9,), ())
    assert child.exit_kind == "callback_request"
    if mutation == "ancestor_control":
        target, offset = owner.control, parent_sp - CONTROL_BASE
    else:
        spec = parent if mutation == "ancestor_code" else child_spec
        target, offset = owner.ram, owner.labels[id(spec)]["stub"]
    original = target[offset]
    target[offset] ^= 1
    before, receipt = owner.snapshot(), _receipt(owner.runner)
    with pytest.raises((ValueError, RuntimeError)):
        owner.runner.resume_callback_v3(child.token, (99,))
    assert owner.snapshot() == before and _receipt(owner.runner) == receipt
    target[offset] = original
    done = owner.runner.resume_callback_v3(child.token, (9,))
    assert done.outputs == (9,) and done.segment_id == child.segment_id + 1
    assert owner.runner.resume_callback_v3(first.token, done.outputs).outputs == (9,)
    owner.runner.close()


@pytest.mark.parametrize("callback", [False, True])
def test_real_delivery_failure_after_child_pop_cancels_root_and_keeps_child_prefix_receipt(callback):
    owner = Owner()
    source = "st.b r4, r5\n" + (CALL if callback else "mov r4, r5\nret.l")
    parent, child_spec, edge = owner.publish_pair(source, child_inputs=2,
        child_callbacks=(("call", "stub", 2, 1, 1),) if callback else (), child_limit=int(callback))
    owner.runner._test_fail_marshalling_after_results(3 if callback else 2)
    first = owner.begin(parent, spans=((EXT_BASE, 1, "write"),))
    parent_view, parent_bytes = _view(owner.state), bytes(owner.control[:STACK_SIZE])
    before, receipt = owner.snapshot(), _receipt(owner.runner)
    with pytest.raises(ValueError):
        owner.runner.begin_child_v3(first.token, edge, (), ())
    assert owner.snapshot() == before and _receipt(owner.runner) == receipt
    if callback:
        child = owner.runner.begin_child_v3(first.token, edge, (EXT_BASE, 90), ((EXT_BASE, 1, "write"),))
        assert child.exit_kind == "callback_request" and child.instructions == 3
        with pytest.raises(MemoryError):
            owner.runner.resume_callback_v3(child.token, (9,))
    else:
        with pytest.raises(MemoryError):
            owner.runner.begin_child_v3(first.token, edge, (EXT_BASE, 90), ((EXT_BASE, 1, "write"),))
    receipt = owner.runner.last_segment_v3()
    assert receipt.parent_invocation_id == first.invocation_id and receipt.depth == 2
    assert receipt.root_invocation_id == first.invocation_id and receipt.invocation_id > first.invocation_id
    assert receipt.invocation_started is (not callback) and not receipt.callback_request
    assert receipt.instructions == (2 if callback else 3)
    assert receipt.invocation_instructions == (5 if callback else 3)
    assert receipt.chain_instructions == 2 + receipt.invocation_instructions
    assert receipt.chain_callbacks == (2 if callback else 1)
    assert receipt.segment_id == (3 if callback else 2)
    assert owner.external[0] == 90
    assert _view(owner.state) == parent_view  # The successful child was popped before py::cast failed.
    assert bytes(owner.control[:STACK_SIZE]) == parent_bytes
    assert owner.state.cycle_count == receipt.chain_cycles
    before, retained = owner.snapshot(), _receipt(owner.runner)
    assert owner.runner.cancel_chain_v3() is None
    assert owner.snapshot() == before and _receipt(owner.runner) == retained
    with pytest.raises((ValueError, RuntimeError)):
        owner.runner.resume_callback_v3(first.token, (99,))
    if callback:
        with pytest.raises((ValueError, RuntimeError)):
            owner.runner.resume_callback_v3(child.token, (99,))
    owner.state.set_reg(4, 77)
    retry = owner.begin(parent)
    assert retry.invocation_id > receipt.invocation_id and retry.segment_id == receipt.segment_id + 1
    assert owner.runner.resume_callback_v3(retry.token, (7,)).outputs == (7,)
    owner.runner.close()


@pytest.mark.parametrize("cleanup", ["cancel", "close"])
def test_nested_cleanup_retires_every_token_without_return_work_and_blocks_legacy_until_retirement(cleanup):
    owner = Owner()
    parent, child_spec, edge = owner.publish_pair(CALL, child_callbacks=(("call", "stub", 2, 1, 1),),
                                                child_limit=1)
    first = owner.begin(parent)
    child = owner.runner.begin_child_v3(first.token, edge, (9,), ())
    before, receipt = owner.snapshot(), _receipt(owner.runner)
    for operation in (owner.legacy.close, owner.legacy.cancel_invocation,
                      lambda: owner.runner.publish_code_v3(parent),
                      lambda: owner.runner.revoke_code_v3(child_spec),
                      lambda: owner.begin(parent), lambda: owner.state.set_reg(4, 99),
                      lambda: owner.state.icache_reset()):
        with pytest.raises(RuntimeError):
            operation()
        assert owner.snapshot() == before and _receipt(owner.runner) == receipt
    clone = native.RoutineSpecV3(**{name: getattr(child_spec, name) for name in (
        "code_base", "code", "entry_offset", "input_cells", "output_cells", "stack_base", "stack_size",
        "max_instructions", "max_callback_requests", "callbacks")})
    assert owner.runner.is_code_published_v3(parent) and owner.runner.is_code_published_v3(child_spec)
    assert not owner.runner.is_code_published_v3(clone)
    assert owner.snapshot() == before and _receipt(owner.runner) == receipt
    if cleanup == "cancel":
        cancelled = owner.runner.cancel_chain_v3(child.token)
        assert cancelled.exit_kind == "cancelled" and cancelled.depth == 2
        assert cancelled.instructions == cancelled.cycles == 0 and cancelled.chain_instructions == 4
        assert owner.runner.cancel_chain_v3() is None
    else:
        owner.runner.close()
    assert owner.snapshot() == before and _receipt(owner.runner) == receipt
    for token in (first.token, child.token):
        with pytest.raises((ValueError, RuntimeError)):
            owner.runner.resume_callback_v3(token, (99,))
    owner.state.set_reg(4, 77)
    if cleanup == "cancel":
        owner.runner.close()
    owner.control.extend(b"released")
    owner.state.attach_mem(bytearray(RAM_SIZE), RAM_SIZE)
