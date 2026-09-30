"""One-frame task admission and quanta preserve ordinary native execution."""

from __future__ import annotations

import copy
import pickle

import _mp64_accel as native
import pytest

from asm import assemble
from tests.test_native_hybrid_nested_root import (
    BUFFER, CODE_BASE, CONTROL_BASE, CONTROL_SIZE, EXT_BASE, EXT_SIZE, LOOP, MASK64,
    OrdinaryReference, RAM_SIZE, SINGLE,
)


BUDGET_FIELDS = (
    "invocation_instructions_remaining", "root_instructions_remaining",
    "invocation_callbacks_remaining", "root_callbacks_remaining", "quantum_instructions",
)
RECEIPT_FIELDS = (
    "root_generation", "root_id", "invocation_id", "parent_invocation_id", "depth", "sequence",
    "invocation_started", "root_entries", "state", "instructions", "cycles", "callback_requests",
    "invocation_instructions", "invocation_cycles", "invocation_callbacks", "root_instructions",
    "root_cycles", "root_callbacks",
)
SPEC_FIELDS = (
    "code_base", "code", "entry_offset", "input_cells", "output_cells", "stack_base", "stack_size",
    "max_instructions", "max_callback_requests", "callbacks",
)


def budget(quantum=0, *, own=100, root=1000, callbacks=100, root_callbacks=1024):
    return native.TaskBudgetV1(own, root, callbacks, root_callbacks, quantum)


def receipt(value):
    return None if value is None else tuple(getattr(value, name) for name in RECEIPT_FIELDS)


class Harness:
    def __init__(self, source=SINGLE, *, inputs=2, outputs=1, callbacks=(("call", "stub", 7, 2, 1),),
                 instructions=100, callback_limit=1, publish=True, shared=True, entry=0):
        labels = self.labels = {}
        raw = bytes(assemble(source, base_addr=CODE_BASE, labels_out=labels))
        code = raw + b"\x01" * (-len(raw) % 16)
        self.ram, self.external = bytearray(RAM_SIZE), bytearray(EXT_SIZE)
        self.control = bytearray(b"\xA5" * CONTROL_SIZE)
        self.ram[CODE_BASE:CODE_BASE + len(code)] = code
        self.state = native.CPUState()
        self.state.attach_mem(self.ram, RAM_SIZE)
        self.state.attach_ext_mem(self.external, EXT_BASE, EXT_SIZE)
        self.state.icache_control_write(1)
        self.private = native.RoutineRunnerV3(self.state, CONTROL_BASE, self.control) if shared else None
        self.runner = self.private.task_v1() if shared else native.TaskRoutineRunnerV1(
            self.state, CONTROL_BASE, self.control)
        self.sites = tuple((labels[call] - CODE_BASE, labels[stub] - CODE_BASE, export, count_in, count_out)
                           for call, stub, export, count_in, count_out in callbacks)
        self.fields = dict(code_base=CODE_BASE, code=code,
            entry_offset=labels[entry] - CODE_BASE if isinstance(entry, str) else entry, input_cells=inputs,
            output_cells=outputs, stack_base=CONTROL_BASE, stack_size=CONTROL_SIZE,
            max_instructions=instructions, max_callback_requests=callback_limit, callbacks=self.sites)
        self.spec = native.TaskRoutineSpecV1(**self.fields)
        if publish:
            self.runner.prepare_code(self.spec)
            assert self.runner.seal_publications(((self.spec, ()),)) == ((),)

    def bind(self, root_id=1, *, instructions=1000, callbacks=1024, entries=1024):
        return self.runner.bind_root(root_id, instructions, callbacks, entry_limit=entries)

    def begin(self, token, arguments=(7, 3), spans=(), *, allowance=None, **kwargs):
        return self.runner.begin(self.spec, arguments, spans, root_token=token,
                                 budget=budget() if allowance is None else allowance, **kwargs)

    def snapshot(self):
        controls = ("psel", "xsel", "spsel", "sw", "d_reg", "q_out", "t_reg", "ef_flags", "halted",
                    "idle", "ext_modifier", "ivt_base", "ivec_id", "trap_addr", "wake_ms",
                    "priv_level", "core_id", "num_cores", "irq_ipi", "icache_enabled")
        return (tuple(self.state.get_reg(index) for index in range(32)), self.state.flags_pack(),
                tuple(getattr(self.state, name) for name in controls), self.state.cycle_count,
                self.state.icache_hits, self.state.icache_misses, self.state.icache_snapshot(),
                bytes(self.ram), bytes(self.external), bytes(self.control))


def assert_receipt(harness, result, *, state, sequence, started=False):
    value = result.receipt
    assert type(value) is native.TaskSegmentReceiptV1
    assert (value.state, value.sequence, value.invocation_started) == (state, sequence, started)
    assert value.parent_invocation_id == 0 and value.depth == 1 and value.root_generation > 0
    assert (result.instructions, result.cycles) == (value.instructions, value.cycles)
    assert receipt(harness.runner.last_receipt()) == receipt(value)
    assert type(result.operation_token) is native.TaskOperationTokenV1
    if state != "callback":
        assert result.request_token is None
    return value


def test_admission_and_every_quantum_match_existing_ordinary_call_return_execution_cold_and_warm():
    harness = Harness(BUFFER, inputs=3)
    reference = OrdinaryReference(harness)
    previous_invocation = 0
    for root_id in (1, 2):
        token = harness.bind(root_id)
        before = harness.snapshot()
        admitted = harness.begin(token, (EXT_BASE, 7, 3), ((EXT_BASE, 8, "write"),))
        value = assert_receipt(harness, admitted, state="yielded", sequence=1, started=True)
        assert value.instructions == value.cycles == value.root_instructions == value.root_cycles == 0
        assert value.root_entries == 1 and value.invocation_id > previous_invocation
        assert harness.snapshot() == before
        unchanged = harness.runner.advance(admitted.operation_token, budget=budget(0))
        assert_receipt(harness, unchanged, state="yielded", sequence=2)
        assert harness.snapshot() == before and unchanged.operation_token is not admitted.operation_token
        reference.begin((EXT_BASE, 7, 3))
        current, sequence = unchanged, 2
        for index in range(7):
            cycles = reference.step()
            current = harness.runner.advance(current.operation_token, budget=budget(1))
            sequence += 1
            assert (current.instructions, current.cycles) == (1, cycles)
            assert_receipt(harness, current, state="callback" if index == 6 else "yielded", sequence=sequence)
            reference.assert_equal()
        assert current.exit_kind == "callback_request" and current.arguments == (7, 3)
        assert (current.site, current.request_sequence, current.export_id) == (0, 1, 7)
        assert type(current.request_token) is native.TaskRequestTokenV1
        assert current.receipt.callback_requests == current.receipt.invocation_callbacks == 1
        before = harness.snapshot()
        staged = harness.runner.reply(current.request_token, (MASK64 - 2,), budget=budget(0))
        sequence += 1
        assert_receipt(harness, staged, state="yielded", sequence=sequence)
        assert staged.instructions == staged.cycles == 0 and harness.snapshot() == before
        again = harness.runner.advance(staged.operation_token, budget=budget(0))
        sequence += 1
        assert_receipt(harness, again, state="yielded", sequence=sequence)
        assert harness.snapshot() == before
        reference.reply((MASK64 - 2,))
        current = again
        for index in range(3):
            cycles = reference.step()
            current = harness.runner.advance(current.operation_token, budget=budget(1))
            sequence += 1
            assert (current.instructions, current.cycles) == (1, cycles)
            assert_receipt(harness, current, state="returned" if index == 2 else "yielded", sequence=sequence)
            reference.assert_equal()
        assert current.exit_kind == "returned" and current.outputs == (MASK64 - 2,)
        assert current.receipt.invocation_instructions == current.receipt.root_instructions == 10
        assert current.receipt.invocation_cycles == current.receipt.root_cycles == reference.cycles
        assert current.receipt.root_callbacks == 1
        assert int.from_bytes(harness.external[:8], "little") == MASK64 - 2
        retained = receipt(harness.runner.last_receipt())
        cancellation = harness.runner.cancel_all()
        assert cancellation.retired_invocation_ids == () and receipt(cancellation.receipt) == retained
        assert receipt(harness.runner.last_receipt()) == retained
        previous_invocation = current.receipt.invocation_id
    harness.runner.close()


def test_zero_quantum_admission_preserves_arbitrary_cpu_control_and_sentinel_bytes():
    harness = Harness()
    for index in range(32):
        harness.state.set_reg(index, 0xB000 + index)
    harness.state.psel, harness.state.xsel, harness.state.spsel = 8, 9, 10
    harness.state.sw = 7
    harness.state.flags_unpack(255)
    harness.state.d_reg = 17
    harness.state.ext_modifier = 0
    harness.state.halted = harness.state.idle = True
    token = harness.bind()
    before = harness.snapshot()
    admitted = harness.begin(token)
    assert harness.snapshot() == before
    for _ in range(3):
        admitted = harness.runner.advance(admitted.operation_token, budget=budget(0))
        assert admitted.receipt.state == "yielded" and admitted.instructions == admitted.cycles == 0
        assert harness.snapshot() == before
    cancelled = harness.runner.cancel_suffix(admitted.operation_token)
    assert cancelled.retired_invocation_ids == (admitted.receipt.invocation_id,)
    assert harness.snapshot() == before
    harness.runner.close()


@pytest.mark.parametrize("field,maximum", tuple(zip(BUDGET_FIELDS, (1000000, 10000000, 1024, 1024, 1000000))))
@pytest.mark.parametrize("invalid", [True, -1, "1", 1.0, "above"])
def test_budget_fields_are_exact_immutable_bounded_values(field, maximum, invalid):
    fields = dict(zip(BUDGET_FIELDS, (100, 1000, 100, 1000, 0)))
    fields[field] = maximum + 1 if invalid == "above" else invalid
    with pytest.raises((TypeError, ValueError)):
        native.TaskBudgetV1(**fields)
    valid = budget()
    with pytest.raises(AttributeError):
        setattr(valid, field, 1)


@pytest.mark.parametrize("fault", ["positive_quantum", "arity", "cell_type", "span", "protected",
                                   "zero_own", "zero_root", "foreign_root", "unpublished"])
def test_begin_preflight_changes_no_cpu_control_receipt_or_entry_count(fault):
    harness = Harness(publish=fault != "unpublished")
    other = Harness()
    token, foreign = harness.bind(), other.bind()
    args, spans, allowance, options = (7, 3), (), budget(), {}
    if fault == "positive_quantum":
        allowance = budget(1)
    elif fault == "arity":
        args = ()
    elif fault == "cell_type":
        args = (True, 3)
    elif fault == "span":
        spans = ((CONTROL_BASE, 8, "write"),)
    elif fault == "protected":
        spans, options = ((EXT_BASE, 8, "write"),), {"protected_spans": ((EXT_BASE, 8),)}
    elif fault == "zero_own":
        allowance = budget(own=0)
    elif fault == "zero_root":
        allowance = budget(root=0)
    elif fault == "foreign_root":
        token = foreign
    before = harness.snapshot()
    with pytest.raises((TypeError, ValueError, RuntimeError)):
        harness.begin(token, args, spans, allowance=allowance, **options)
    assert harness.snapshot() == before and harness.runner.last_receipt() is None
    if fault == "unpublished":
        harness.runner.prepare_code(harness.spec)
        harness.runner.seal_publications(((harness.spec, ()),))
    admitted = harness.begin(token if fault != "foreign_root" else harness.bind(2))
    assert admitted.receipt.sequence == admitted.receipt.root_entries == admitted.receipt.invocation_id == 1
    harness.runner.cancel_all()
    harness.runner.close()
    other.runner.close()


def test_operation_request_and_root_tokens_are_exact_one_shot_and_not_serializable():
    harness, other = Harness(), Harness()
    root, foreign_root = harness.bind(), other.bind()
    admitted, foreign_admitted = harness.begin(root), other.begin(foreign_root)
    yielded = harness.runner.advance(admitted.operation_token, budget=budget(0))
    before, retained = harness.snapshot(), receipt(harness.runner.last_receipt())
    for token in (admitted.operation_token, foreign_admitted.operation_token):
        with pytest.raises((ValueError, RuntimeError)):
            harness.runner.advance(token, budget=budget(1))
        with pytest.raises((ValueError, RuntimeError)):
            harness.runner.cancel_suffix(token)
        assert harness.snapshot() == before and receipt(harness.runner.last_receipt()) == retained
    callback = harness.runner.advance(yielded.operation_token, budget=budget(100))
    assert callback.receipt.state == "callback"
    before, retained = harness.snapshot(), receipt(harness.runner.last_receipt())
    with pytest.raises((ValueError, RuntimeError)):
        harness.runner.advance(callback.operation_token, budget=budget(100))
    with pytest.raises(ValueError):
        harness.runner.reply(callback.request_token, (), budget=budget(0))
    assert harness.snapshot() == before and receipt(harness.runner.last_receipt()) == retained
    for token in (root, callback.operation_token, callback.request_token):
        with pytest.raises(TypeError):
            type(token)()
        for duplicate in (copy.copy, copy.deepcopy, pickle.dumps):
            with pytest.raises((TypeError, RuntimeError)):
                duplicate(token)
    staged = harness.runner.reply(callback.request_token, (9,), budget=budget(0))
    before, retained = harness.snapshot(), receipt(harness.runner.last_receipt())
    with pytest.raises((ValueError, RuntimeError)):
        harness.runner.reply(callback.request_token, (99,), budget=budget(0))
    with pytest.raises((ValueError, RuntimeError)):
        harness.runner.cancel_suffix(callback.operation_token)
    assert harness.snapshot() == before and receipt(harness.runner.last_receipt()) == retained
    done = harness.runner.advance(staged.operation_token, budget=budget(100))
    assert done.outputs == (9,) and done.receipt.state == "returned"
    with pytest.raises((ValueError, RuntimeError)):
        harness.runner.advance(done.operation_token, budget=budget(1))
    with pytest.raises((ValueError, RuntimeError)):
        harness.runner.cancel_suffix(done.operation_token)
    harness.runner.close()
    other.runner.close()


@pytest.mark.parametrize("exhaustion", ["own", "root"])
def test_exhausted_lowered_fuel_is_terminal_even_at_zero_quantum_without_initializing_cpu(exhaustion):
    harness = Harness()
    root = harness.bind()
    admitted = harness.begin(root)
    before = harness.snapshot()
    failed = harness.runner.advance(admitted.operation_token,
        budget=budget(0, own=0 if exhaustion == "own" else 100, root=0 if exhaustion == "root" else 1000))
    assert failed.exit_kind == "instruction_limit" and failed.receipt.state == "failed"
    assert failed.instructions == failed.cycles == failed.receipt.root_instructions == 0
    assert failed.receipt.sequence == 2 and harness.snapshot() == before
    with pytest.raises((ValueError, RuntimeError)):
        harness.runner.advance(failed.operation_token, budget=budget(100))
    with pytest.raises((ValueError, RuntimeError)):
        harness.begin(root)
    retained = receipt(harness.runner.last_receipt())
    cancelled = harness.runner.cancel_suffix(failed.operation_token)
    assert cancelled.retired_invocation_ids == (failed.receipt.invocation_id,)
    assert receipt(cancelled.receipt) == retained and receipt(harness.runner.last_receipt()) == retained
    assert harness.snapshot() == before
    if exhaustion == "root":
        with pytest.raises(ValueError):
            harness.begin(root)
    else:
        next_entry = harness.begin(root)
        assert next_entry.receipt.root_entries == 2 and next_entry.receipt.sequence == 3
        harness.runner.cancel_all()
    harness.runner.close()


def test_staged_reply_is_not_published_when_a_later_budget_lowers_remaining_fuel_to_zero():
    harness = Harness()
    root = harness.bind()
    admitted = harness.begin(root)
    callback = harness.runner.advance(admitted.operation_token, budget=budget(3))
    assert callback.receipt.state == "callback" and callback.instructions == 3
    before = harness.snapshot()
    staged = harness.runner.reply(callback.request_token, (99,), budget=budget(0))
    assert staged.receipt.state == "yielded" and harness.snapshot() == before
    failed = harness.runner.advance(staged.operation_token, budget=budget(0, own=0))
    assert failed.exit_kind == "instruction_limit" and failed.receipt.state == "failed"
    assert failed.instructions == failed.cycles == 0 and failed.receipt.invocation_instructions == 3
    assert harness.snapshot() == before and harness.state.get_reg(4) == 7
    harness.runner.cancel_all()
    harness.runner.close()


def test_remaining_ceiling_cannot_be_renewed_after_a_zero_quantum_lowering():
    harness = Harness("inc r4\ninc r4\nret.l", inputs=1, callbacks=(), callback_limit=0)
    root = harness.bind()
    first = harness.begin(root, (7,))
    lower = harness.runner.advance(first.operation_token, budget=budget(0, own=2, root=2))
    failed = harness.runner.advance(lower.operation_token, budget=budget(100, own=100, root=1000))
    assert failed.exit_kind == "instruction_limit" and failed.instructions == 2
    assert harness.state.get_reg(4) == 9 and failed.outputs == ()
    assert failed.receipt.root_instructions == 2
    harness.runner.cancel_all()
    with pytest.raises(ValueError):
        harness.begin(root, (7,))
    harness.runner.close()


def test_empty_chain_reentry_spends_original_root_entries_fuel_and_sequence_until_new_root():
    harness = Harness("ret.l", inputs=1, callbacks=(), callback_limit=0)
    root = harness.bind(instructions=2, entries=2)
    for index in range(2):
        admitted = harness.begin(root, (index,))
        assert admitted.receipt.sequence == index * 2 + 1 and admitted.receipt.root_entries == index + 1
        done = harness.runner.advance(admitted.operation_token, budget=budget(1))
        assert done.outputs == (index,) and done.receipt.sequence == index * 2 + 2
        assert done.receipt.root_instructions == index + 1
        cancelled = harness.runner.cancel_all()
        assert cancelled.retired_invocation_ids == ()
    before, retained = harness.snapshot(), receipt(harness.runner.last_receipt())
    with pytest.raises(ValueError):
        harness.begin(root, (9,))
    with pytest.raises(ValueError):
        harness.bind(1)
    assert harness.snapshot() == before and receipt(harness.runner.last_receipt()) == retained
    newer = harness.bind(2, instructions=1, entries=1)
    with pytest.raises((ValueError, RuntimeError)):
        harness.begin(root, (9,))
    admitted = harness.begin(newer, (9,))
    assert admitted.receipt.root_generation > done.receipt.root_generation
    assert admitted.receipt.sequence == admitted.receipt.root_entries == 1
    assert admitted.receipt.root_instructions == 0 and admitted.receipt.invocation_id > done.receipt.invocation_id
    assert harness.runner.advance(admitted.operation_token, budget=budget(1)).outputs == (9,)
    harness.runner.close()


def test_entry_limit_is_retained_after_cancellation_without_work():
    harness = Harness()
    root = harness.bind(entries=1)
    admitted = harness.begin(root)
    cancelled = harness.runner.cancel_all()
    assert cancelled.retired_invocation_ids == (admitted.receipt.invocation_id,)
    before, retained = harness.snapshot(), receipt(harness.runner.last_receipt())
    with pytest.raises(ValueError):
        harness.begin(root)
    assert harness.snapshot() == before and receipt(harness.runner.last_receipt()) == retained
    harness.runner.close()


def test_completed_call_on_last_quantum_is_callback_and_later_callback_limit_keeps_real_prefix():
    harness = Harness(LOOP, callback_limit=2)
    root = harness.bind(callbacks=1)
    admitted = harness.begin(root)
    callback = harness.runner.advance(admitted.operation_token, budget=budget(3))
    assert callback.receipt.state == "callback" and callback.instructions == 3
    failed = harness.runner.reply(callback.request_token, (9,), budget=budget(100))
    assert failed.exit_kind == "callback_limit" and failed.receipt.state == "failed"
    assert failed.instructions == 4 and failed.receipt.root_instructions == 7
    assert failed.receipt.callback_requests == 0 and failed.receipt.root_callbacks == 1
    assert failed.request_token is None and failed.outputs == ()
    slot = harness.state.get_reg(15) - CONTROL_BASE
    assert int.from_bytes(harness.control[slot:slot + 8], "little") == harness.labels["after"]
    assert harness.state.get_reg(3) == harness.labels["stub"]
    harness.runner.cancel_all()
    harness.runner.close()


def test_preparation_is_nonexecuting_and_atomic_empty_edge_seal_keeps_failed_batch_unchanged():
    harness = Harness("ret.l", inputs=1, callbacks=(), callback_limit=0, publish=False)
    second_fields = dict(harness.fields, code_base=CODE_BASE + 0x100)
    second = native.TaskRoutineSpecV1(**second_fields)
    harness.ram[second.code_base:second.code_base + len(second.code)] = second.code
    legacy = harness.private.legacy_v2()
    old = native.RoutineSpecV1(CODE_BASE, len(harness.spec.code), 0, 1, 1, CONTROL_BASE, CONTROL_SIZE, 100)
    legacy.publish_code(old)
    assert legacy.run(old, (7,), (), 100).outputs == (7,)
    before = harness.snapshot()
    assert harness.runner.prepare_code(harness.spec) is None
    assert harness.runner.prepare_code(second) is None
    assert harness.snapshot() == before
    assert harness.runner.is_code_registered(harness.spec) and harness.runner.is_code_registered(second)
    assert not harness.runner.is_code_published(harness.spec)
    root = harness.bind()
    with pytest.raises(ValueError):
        harness.begin(root, (7,))
    assert harness.snapshot() == before and harness.runner.last_receipt() is None
    harness.ram[second.code_base] ^= 1
    before = harness.snapshot()
    with pytest.raises(ValueError):
        harness.runner.seal_publications(((harness.spec, ()), (second, ())))
    assert harness.snapshot() == before and harness.runner.last_receipt() is None
    assert not harness.runner.is_code_published(harness.spec) and not harness.runner.is_code_published(second)
    harness.ram[second.code_base] ^= 1
    assert harness.runner.seal_publications(((harness.spec, ()), (second, ()))) == ((), ())
    assert harness.runner.is_code_published(harness.spec) and harness.runner.is_code_published(second)
    assert harness.runner.seal_publications(((harness.spec, ()),)) == ((),)
    accepted = harness.begin(root, (9,))
    assert harness.runner.advance(accepted.operation_token, budget=budget(1)).outputs == (9,)
    harness.runner.close()


@pytest.mark.parametrize("shape", ["list_batch", "list_pair", "list_edges", "duplicate", "unprepared"])
def test_publication_rejects_nonexact_batches_without_partial_seals(shape):
    harness = Harness(publish=False)
    harness.runner.prepare_code(harness.spec)
    clone = native.TaskRoutineSpecV1(**harness.fields)
    batch = {
        "list_batch": [(harness.spec, ())],
        "list_pair": ([harness.spec, ()],),
        "list_edges": ((harness.spec, []),),
        "duplicate": ((harness.spec, ()), (harness.spec, ())),
        "unprepared": ((harness.spec, ()), (clone, ())),
    }[shape]
    before = harness.snapshot()
    with pytest.raises((TypeError, ValueError)):
        harness.runner.seal_publications(batch)
    assert harness.snapshot() == before and harness.runner.last_receipt() is None
    assert harness.runner.is_code_registered(harness.spec) and not harness.runner.is_code_published(harness.spec)
    assert not harness.runner.is_code_registered(clone)
    assert harness.runner.seal_publications(((harness.spec, ()),)) == ((),)
    harness.runner.close()


def test_prepared_task_entries_share_common_publication_capacity_with_private_transport():
    harness = Harness("ret.l", inputs=1, callbacks=(), callback_limit=0, publish=False)
    private_spec = native.RoutineSpecV2(**{name: value for name, value in harness.fields.items()
                                         if name != "max_callback_requests"})
    harness.private.legacy_v2().publish_code_v2(private_spec)
    prepared = [native.TaskRoutineSpecV1(**harness.fields) for _ in range(64)]
    for spec in prepared[:63]:
        harness.runner.prepare_code(spec)
    before = harness.snapshot()
    with pytest.raises(ValueError):
        harness.runner.prepare_code(prepared[-1])
    assert harness.snapshot() == before and not harness.runner.is_code_registered(prepared[-1])
    harness.runner.revoke_code(prepared[0])
    assert not harness.runner.is_code_registered(prepared[0])
    harness.runner.prepare_code(prepared[-1])
    assert harness.runner.is_code_registered(prepared[-1])
    harness.runner.close()


@pytest.mark.parametrize("phase", ["admitted", "staged_reply"])
def test_stale_code_rejects_before_initialization_or_staged_output_publication_and_allows_retry(phase):
    harness = Harness()
    root = harness.bind()
    current = harness.begin(root)
    if phase == "staged_reply":
        callback = harness.runner.advance(current.operation_token, budget=budget(100))
        current = harness.runner.reply(callback.request_token, (99,), budget=budget(0))
    address = harness.labels["stub"]
    original = harness.ram[address]
    harness.ram[address] ^= 1
    before, retained = harness.snapshot(), receipt(harness.runner.last_receipt())
    with pytest.raises((ValueError, RuntimeError)):
        harness.runner.advance(current.operation_token, budget=budget(100))
    assert harness.snapshot() == before and receipt(harness.runner.last_receipt()) == retained
    harness.ram[address] = original
    result = harness.runner.advance(current.operation_token, budget=budget(100))
    if phase == "admitted":
        assert result.receipt.state == "callback"
        result = harness.runner.reply(result.request_token, (99,), budget=budget(100))
    assert result.outputs == (99,) and result.receipt.state == "returned"
    harness.runner.close()


def test_failed_access_retains_exact_ordinary_prefix_and_live_failure_until_cancel():
    harness = Harness("ldi r6, 90\nst.b r4, r6\naddi r4, 8\ndenied:\nstr r4, r6\nret.l",
                      inputs=1, outputs=0, callbacks=(), callback_limit=0)
    reference = OrdinaryReference(harness)
    reference.begin((EXT_BASE,))
    for _ in range(3):
        reference.step()
    root = harness.bind()
    admitted = harness.begin(root, (EXT_BASE,), ((EXT_BASE, 1, "write"),))
    failed = harness.runner.advance(admitted.operation_token, budget=budget(100))
    assert failed.exit_kind == "rejected_access" and failed.receipt.state == "failed"
    assert (failed.instructions, failed.cycles) == (3, reference.cycles)
    assert failed.receipt.root_instructions == failed.receipt.invocation_instructions == 3
    assert failed.instruction_pc == harness.labels["denied"]
    assert (failed.access_address, failed.access_width, failed.access_operation) == (EXT_BASE + 8, 8, "write")
    assert failed.outputs == () and bytes(harness.external) == bytes(reference.memory[:EXT_SIZE])
    assert bytes(harness.control) == bytes(reference.memory[EXT_SIZE:])
    assert harness.external[0] == 90 and harness.state.get_reg(4) == EXT_BASE + 8
    assert harness.state.cycle_count == reference.state.cycle_count
    before, retained = harness.snapshot(), receipt(harness.runner.last_receipt())
    with pytest.raises(RuntimeError):
        harness.state.set_reg(4, 99)
    with pytest.raises((ValueError, RuntimeError)):
        harness.runner.advance(failed.operation_token, budget=budget(100))
    assert harness.snapshot() == before and receipt(harness.runner.last_receipt()) == retained
    cancelled = harness.runner.cancel_suffix(failed.operation_token)
    assert type(cancelled) is native.TaskCancellationV1
    assert cancelled.retired_invocation_ids == (failed.receipt.invocation_id,)
    assert cancelled.surviving_parent_id == 0 and cancelled.surviving_parent_token is None
    assert receipt(cancelled.receipt) == retained and harness.snapshot() == before
    harness.state.set_reg(4, 99)
    harness.runner.close()


@pytest.mark.parametrize("shared", [False, True])
def test_task_facade_does_not_inherit_private_authority_and_one_owner_closes_all_views(shared):
    harness = Harness(shared=shared)
    assert not hasattr(native, "HYBRID_TASK_ROUTINE_ABI_VERSION")
    assert type(harness.runner) is native.TaskRoutineRunnerV1
    assert not isinstance(harness.runner, (native.RoutineRunnerV1, native.RoutineRunnerV2, native.RoutineRunnerV3))
    assert type(harness.spec) is native.TaskRoutineSpecV1
    assert not isinstance(harness.spec, (native.RoutineSpecV1, native.RoutineSpecV2, native.RoutineSpecV3))
    if shared:
        assert harness.private.task_v1() is harness.runner
        with pytest.raises(TypeError):
            harness.private.publish_code_v3(harness.spec)
    with pytest.raises(BufferError):
        harness.control.extend(b"pinned")
    root = harness.bind()
    admitted = harness.begin(root)
    retained = receipt(harness.runner.last_receipt())
    harness.runner.close()
    assert receipt(harness.runner.last_receipt()) == retained
    with pytest.raises(RuntimeError):
        harness.runner.advance(admitted.operation_token, budget=budget(1))
    if shared:
        with pytest.raises(RuntimeError):
            harness.private.is_code_published_v3(native.RoutineSpecV3(**harness.fields))
    harness.control.extend(b"released")
    harness.state.attach_mem(bytearray(RAM_SIZE), RAM_SIZE)


def test_private_execution_between_empty_task_invocations_does_not_rebase_task_receipts():
    harness = Harness("ret.l", inputs=1, callbacks=(), callback_limit=0)
    legacy = harness.private.legacy_v2()
    private_spec = native.RoutineSpecV2(**{name: value for name, value in harness.fields.items()
                                         if name != "max_callback_requests"})
    legacy.publish_code_v2(private_spec)
    root = harness.bind()
    first = harness.begin(root, (7,))
    one = harness.runner.advance(first.operation_token, budget=budget(1))
    assert one.receipt.root_instructions == 1
    retained = receipt(harness.runner.last_receipt())
    old = legacy.begin_v2(private_spec, (17,), (), 100)
    assert old.outputs == (17,) and old.instructions == 1
    assert receipt(harness.runner.last_receipt()) == retained
    second = harness.begin(root, (9,))
    two = harness.runner.advance(second.operation_token, budget=budget(1))
    assert two.outputs == (9,) and two.receipt.sequence == 4 and two.receipt.root_entries == 2
    assert (two.receipt.root_instructions, two.receipt.root_cycles) == (2, 4)
    assert harness.state.cycle_count == 6
    assert legacy.last_segment_v2().segment_id == old.segment_id
    harness.runner.close()


@pytest.mark.parametrize("failure_after", [1, 2, 3])
def test_real_task_marshalling_failure_retains_receipt_and_blocks_owner_until_all_cancel(failure_after):
    harness = Harness()
    root = harness.bind()
    harness.runner._test_fail_marshalling_after_results(failure_after)
    if failure_after == 1:
        before = harness.snapshot()
        with pytest.raises(MemoryError):
            harness.begin(root)
        assert harness.snapshot() == before
    else:
        admitted = harness.begin(root)
        if failure_after == 2:
            with pytest.raises(MemoryError):
                harness.runner.advance(admitted.operation_token, budget=budget(100))
        else:
            callback = harness.runner.advance(admitted.operation_token, budget=budget(100))
            with pytest.raises(MemoryError):
                harness.runner.reply(callback.request_token, (9,), budget=budget(100))
    latest = harness.runner.last_receipt()
    assert latest.sequence == failure_after
    assert latest.state == ("yielded", "callback", "returned")[failure_after - 1]
    assert latest.root_instructions == (0, 3, 5)[failure_after - 1]
    assert latest.root_entries == 1
    before, retained = harness.snapshot(), receipt(latest)
    for operation in (lambda: harness.begin(root), lambda: harness.bind(2),
                      lambda: harness.state.set_reg(4, 77), harness.private.close, harness.private.cancel_chain_v3,
                      harness.private.legacy_v2().cancel_invocation):
        with pytest.raises(RuntimeError):
            operation()
        assert harness.snapshot() == before and receipt(harness.runner.last_receipt()) == retained
    cancelled = harness.runner.cancel_all()
    assert cancelled.retired_invocation_ids == (() if failure_after == 3 else (latest.invocation_id,))
    assert receipt(cancelled.receipt) == retained and harness.snapshot() == before
    harness.state.set_reg(4, 77)
    next_entry = harness.begin(root)
    assert next_entry.receipt.sequence == failure_after + 1 and next_entry.receipt.root_entries == 2
    assert next_entry.receipt.invocation_id > latest.invocation_id
    harness.runner.cancel_all()
    harness.runner.close()


@pytest.mark.parametrize("field,bad", [("root_id", 0), ("root_id", True), ("root_id", MASK64 + 1),
    ("instruction_limit", 0), ("instruction_limit", 10000001), ("instruction_limit", "1"),
    ("callback_limit", -1), ("callback_limit", 1025), ("callback_limit", False),
    ("entry_limit", 0), ("entry_limit", 1025), ("entry_limit", 1.0)])
def test_root_limits_reject_before_issuing_or_changing_any_execution_state(field, bad):
    harness = Harness()
    values = dict(root_id=1, instruction_limit=100, callback_limit=1, entry_limit=1)
    values[field] = bad
    before = harness.snapshot()
    with pytest.raises((TypeError, ValueError)):
        harness.runner.bind_root(**values)
    assert harness.snapshot() == before and harness.runner.last_receipt() is None
    root = harness.bind()
    assert type(root) is native.TaskRootTokenV1
    admitted = harness.begin(root)
    assert admitted.receipt.root_generation == 1 and admitted.receipt.sequence == 1
    harness.runner.cancel_all()
    harness.runner.close()


def test_admitted_task_excludes_private_mutations_and_has_only_exact_readonly_publication_queries():
    harness = Harness()
    legacy = harness.private.legacy_v2()
    old_fields = {name: value for name, value in harness.fields.items() if name != "max_callback_requests"}
    old, private = native.RoutineSpecV2(**old_fields), native.RoutineSpecV3(**harness.fields)
    legacy.publish_code_v2(old)
    harness.private.publish_code_v3(private)
    root = harness.bind()
    admitted = harness.begin(root)
    before, retained = harness.snapshot(), receipt(harness.runner.last_receipt())
    clone = native.TaskRoutineSpecV1(**harness.fields)
    assert harness.runner.is_code_registered(harness.spec) and harness.runner.is_code_published(harness.spec)
    assert not harness.runner.is_code_registered(clone) and not harness.runner.is_code_published(clone)
    assert harness.snapshot() == before and receipt(harness.runner.last_receipt()) == retained
    for operation in (lambda: legacy.begin_v2(old, (7, 3), (), 100),
                      lambda: harness.private.begin_root_v3(private, (7, 3), (), 100),
                      legacy.cancel_invocation, legacy.close, harness.private.cancel_chain_v3, harness.private.close,
                      lambda: harness.private.is_code_published_v3(private),
                      lambda: harness.runner.prepare_code(clone),
                      lambda: harness.runner.seal_publications(((harness.spec, ()),)),
                      lambda: harness.runner.revoke_code(harness.spec),
                      lambda: harness.bind(2),
                      lambda: harness.runner._test_fail_marshalling_after_results(1),
                      lambda: harness.state.set_reg(4, 99), lambda: harness.state.flags_unpack(0),
                      lambda: harness.state.icache_reset()):
        with pytest.raises(RuntimeError):
            operation()
        assert harness.snapshot() == before and receipt(harness.runner.last_receipt()) == retained
    for name in SPEC_FIELDS:
        with pytest.raises(AttributeError):
            setattr(harness.spec, name, getattr(harness.spec, name))
    for name in RECEIPT_FIELDS:
        with pytest.raises(AttributeError):
            setattr(admitted.receipt, name, getattr(admitted.receipt, name))
    callback = harness.runner.advance(admitted.operation_token, budget=budget(100))
    assert callback.receipt.state == "callback"
    assert harness.runner.reply(callback.request_token, (9,), budget=budget(100)).outputs == (9,)
    harness.runner.close()


@pytest.mark.parametrize("version", [2, 3])
def test_task_routes_cannot_cancel_close_or_enter_a_parked_private_transport(version):
    harness = Harness()
    root = harness.bind()
    if version == 2:
        private = harness.private.legacy_v2()
        spec = native.RoutineSpecV2(**{name: value for name, value in harness.fields.items()
                                      if name != "max_callback_requests"})
        private.publish_code_v2(spec)
        first = private.begin_v2(spec, (7, 3), (), 100)
        resume = private.resume_callback
    else:
        private = harness.private
        spec = native.RoutineSpecV3(**harness.fields)
        private.publish_code_v3(spec)
        first = private.begin_root_v3(spec, (7, 3), (), 100)
        resume = private.resume_callback_v3
    assert first.exit_kind == "callback_request"
    before = harness.snapshot()
    for operation in (harness.runner.close, harness.runner.cancel_all,
                      lambda: harness.begin(root), lambda: harness.bind(2),
                      lambda: harness.runner.is_code_registered(harness.spec),
                      lambda: harness.runner.is_code_published(harness.spec)):
        with pytest.raises(RuntimeError):
            operation()
        assert harness.snapshot() == before and harness.runner.last_receipt() is None
    assert resume(first.token, (9,)).outputs == (9,)
    admitted = harness.begin(root)
    assert admitted.receipt.sequence == 1
    harness.runner.cancel_all()
    harness.runner.close()


def test_reply_on_exhausted_final_call_publishes_failure_without_output_or_return_effect():
    harness = Harness()
    root = harness.bind(instructions=3)
    admitted = harness.begin(root)
    callback = harness.runner.advance(admitted.operation_token, budget=budget(3))
    assert callback.receipt.state == "callback" and callback.instructions == 3
    before = harness.snapshot()
    failed = harness.runner.reply(callback.request_token, (99,), budget=budget(0))
    assert failed.receipt.state == "failed" and failed.exit_kind == "instruction_limit"
    assert failed.instructions == failed.cycles == 0 and failed.receipt.root_instructions == 3
    assert harness.snapshot() == before and harness.state.get_reg(4) == 7
    harness.runner.cancel_all()
    harness.runner.close()


def test_zero_callback_allowance_still_allows_a_callback_free_real_return():
    harness = Harness("ret.l", inputs=1, callbacks=(), callback_limit=0)
    root = harness.bind(callbacks=0)
    admitted = harness.begin(root, (9,), allowance=budget(callbacks=0, root_callbacks=0))
    done = harness.runner.advance(admitted.operation_token, budget=budget(1, callbacks=0, root_callbacks=0))
    assert done.outputs == (9,) and done.receipt.state == "returned"
    assert done.receipt.instructions == 1 and done.receipt.root_callbacks == 0
    harness.runner.close()


def test_zero_work_profile_failure_after_admission_has_its_own_receipt_and_initialized_control():
    harness = Harness(entry="stub")
    root = harness.bind()
    before = harness.snapshot()
    admitted = harness.begin(root)
    assert harness.snapshot() == before
    failed = harness.runner.advance(admitted.operation_token, budget=budget(1))
    assert failed.receipt.state == "failed" and failed.exit_kind == "invalid_callback"
    assert failed.receipt.sequence == 2 and not failed.receipt.invocation_started
    assert failed.instructions == failed.cycles == failed.receipt.root_instructions == 0
    assert harness.state.get_reg(3) == harness.labels["stub"]
    assert bytes(harness.control[-8:]) == b"\xFF" * 8
    assert harness.snapshot() != before
    harness.runner.cancel_all()
    harness.runner.close()


@pytest.mark.parametrize("count", [0, 65, True, False, -1, "1", 1.5, None])
def test_real_delivery_failure_seam_rejects_invalid_arming_without_changing_execution(count):
    harness = Harness()
    root = harness.bind()
    before = harness.snapshot()
    with pytest.raises((TypeError, ValueError)):
        harness.runner._test_fail_marshalling_after_results(count)
    assert harness.snapshot() == before and harness.runner.last_receipt() is None
    admitted = harness.begin(root)
    assert admitted.receipt.state == "yielded"
    harness.runner.cancel_all()
    harness.runner.close()
