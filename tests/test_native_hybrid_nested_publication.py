"""Sealed nested publications bind exact children without entering a machine."""

from __future__ import annotations

import copy
import gc
import pickle

import _mp64_accel as native
import pytest

from asm import assemble


CONTROL_BASE, STACK_SIZE = 0x400000, 128
CONTROL_SIZE = 64 * STACK_SIZE
SPEC_FIELDS = (
    "code_base", "code", "entry_offset", "input_cells", "output_cells",
    "stack_base", "stack_size", "max_instructions", "max_callback_requests", "callbacks",
)


class Owner:
    def __init__(self, *, ram_size=65536):
        self.ram = bytearray(ram_size)
        self.control = bytearray(b"\xA5" * CONTROL_SIZE)
        self.state = native.CPUState()
        self.state.attach_mem(self.ram, len(self.ram))
        self.state.icache_control_write(1)
        self.runner = native.RoutineRunnerV3(self.state, CONTROL_BASE, self.control)
        self.legacy = self.runner.legacy_v2()

    def spec(self, slot=0, *, sites=0, version=3, code_size=16, **changes):
        base = 0x100 + slot * 0x100
        labels = {}
        source = "\n".join(
            f"ldi64 r12, stub{index}\ncall{index}:\ncall.l r12" for index in range(sites)
        )
        source += "\nret.l\n" + "\n".join(f"stub{index}:\nret.l" for index in range(sites))
        raw = bytes(assemble(source, base_addr=base, labels_out=labels))
        size = max(code_size, (len(raw) + 15) & ~15)
        code = raw + b"\x01" * (size - len(raw))
        fields = dict(
            code_base=base, code=code, entry_offset=0, input_cells=1, output_cells=1,
            stack_base=CONTROL_BASE + slot * STACK_SIZE, stack_size=STACK_SIZE,
            max_instructions=100, max_callback_requests=1 if sites else 0,
            callbacks=tuple((labels[f"call{index}"] - base, labels[f"stub{index}"] - base,
                             index, 1, 1) for index in range(sites)),
        )
        fields.update(changes)
        start = fields["code_base"]
        assert start + len(fields["code"]) <= len(self.ram)
        self.ram[start:start + len(fields["code"])] = fields["code"]
        if version == 2:
            fields.pop("max_callback_requests")
        return getattr(native, f"RoutineSpecV{version}")(**fields)

    def snapshot(self):
        return (tuple(self.state.get_reg(index) for index in range(32)),
                self.state.flags_pack(), self.state.psel, self.state.xsel, self.state.spsel,
                self.state.cycle_count, self.state.icache_hits, self.state.icache_misses,
                self.state.icache_snapshot(), bytes(self.ram), bytes(self.control))

    def warm(self, spec):
        ordinary = native.RoutineSpecV1(
            spec.code_base, spec.code_size, spec.entry_offset, spec.input_cells,
            spec.output_cells, spec.stack_base, spec.stack_size, spec.max_instructions,
        )
        self.legacy.publish_code(ordinary)
        result = self.legacy.run(ordinary, (7,), (), 100)
        assert result.exit_kind == "returned" and result.outputs == (7,)


def _clone(spec, **changes):
    fields = {name: getattr(spec, name) for name in SPEC_FIELDS}
    fields.update(changes)
    return native.RoutineSpecV3(**fields)


def _pair(owner, *, sites=1):
    child = owner.spec(1)
    parent = owner.spec(0, sites=sites)
    assert owner.runner.publish_code_v3(child) == ()
    return parent, child


def test_spec_is_distinct_immutable_and_does_not_advertise_nested_execution():
    owner = Owner()
    spec = owner.spec(sites=1, max_callback_requests=1024)
    assert type(spec) is native.RoutineSpecV3
    assert not isinstance(spec, (native.RoutineSpecV1, native.RoutineSpecV2))
    assert spec.max_callback_requests == 1024
    assert type(spec.code) is bytes and type(spec.callbacks) is tuple
    for name in SPEC_FIELDS:
        with pytest.raises(AttributeError):
            setattr(spec, name, getattr(spec, name))
    assert not hasattr(native, "HYBRID_NESTED_ROUTINE_ABI_VERSION")
    for name in ("run", "begin_v2"):
        assert not hasattr(owner.runner, name)
    with pytest.raises(TypeError):
        owner.legacy.publish_code_v2(spec)
    with pytest.raises(TypeError):
        owner.runner.publish_code_v3(owner.spec(1, version=2))
    owner.runner.close()


@pytest.mark.parametrize("limit", [True, False, -1, 1025, 1.0, "1", None])
def test_callback_count_limit_is_an_exact_bounded_integer(limit):
    owner = Owner()
    spec = owner.spec()
    before = owner.snapshot()
    with pytest.raises((TypeError, ValueError)):
        _clone(spec, max_callback_requests=limit)
    assert owner.snapshot() == before
    assert _clone(spec, max_callback_requests=0).max_callback_requests == 0
    owner.runner.close()


def test_spec_requires_its_new_limit_and_exact_immutable_code_bytes():
    owner = Owner()
    spec = owner.spec()
    fields = {name: getattr(spec, name) for name in SPEC_FIELDS}
    with pytest.raises(TypeError):
        native.RoutineSpecV3(**{name: value for name, value in fields.items()
                               if name != "max_callback_requests"})

    class Bytes(bytes):
        pass

    for code in (bytearray(spec.code), memoryview(spec.code), Bytes(spec.code)):
        with pytest.raises(TypeError):
            _clone(spec, code=code)
    owner.runner.close()


@pytest.mark.parametrize("fault", ["entry_operand", "call_operand", "wrong_stub", "shared_stub", "unsupported"])
def test_spec_reuses_the_complete_instruction_and_callback_site_proof(fault):
    owner = Owner()
    spec = owner.spec(sites=2)
    callbacks = [list(site) for site in spec.callbacks]
    code = bytearray(spec.code)
    changes = {}
    if fault == "entry_operand":
        changes["entry_offset"] = 3
    elif fault == "call_operand":
        code[3:5] = bytes(assemble("call.l r12"))
        callbacks[0][0] = 3
    elif fault == "wrong_stub":
        callbacks[0][1] = 0
    elif fault == "shared_stub":
        callbacks[1][1] = callbacks[0][1]
    else:
        code[-1] = bytes(assemble("halt"))[0]
    before = owner.snapshot()
    with pytest.raises(ValueError):
        _clone(spec, code=bytes(code), callbacks=tuple(tuple(row) for row in callbacks), **changes)
    assert owner.snapshot() == before
    owner.runner.close()


def test_exact_rows_issue_stable_opaque_handles_in_order_and_conflicts_do_not_replace_them():
    owner = Owner()
    parent, child = _pair(owner, sites=2)
    rows = ((1, 9, child), (0, 3, child), (1, 3, child))
    edges = owner.runner.publish_code_v3(parent, rows)
    assert type(edges) is tuple and len(edges) == 3
    assert len({id(edge) for edge in edges}) == 3
    assert all(type(edge) is native.ChildEdgeV3 for edge in edges)
    again = owner.runner.publish_code_v3(parent, tuple(tuple(row) for row in rows))
    assert all(left is right for left, right in zip(edges, again))
    with pytest.raises(TypeError):
        native.ChildEdgeV3()
    for edge in edges:
        assert not hasattr(edge, "__dict__")
        for operation in (copy.copy, copy.deepcopy, pickle.dumps):
            with pytest.raises(TypeError):
                operation(edge)
    for changed in (rows[::-1], rows[:-1], ((1, 10, child),) + rows[1:]):
        before = owner.snapshot()
        with pytest.raises(ValueError):
            owner.runner.publish_code_v3(parent, changed)
        assert owner.snapshot() == before
        assert all(a is b for a, b in zip(edges, owner.runner.publish_code_v3(parent, rows)))
    owner.runner.close()


def test_publication_invalidates_its_resident_code_without_running_or_changing_guest_state():
    owner = Owner()
    spec = owner.spec()
    owner.warm(spec)
    before = owner.snapshot()
    assert owner.runner.publish_code_v3(spec) == ()
    after = owner.snapshot()
    assert after[:8] == before[:8] and after[9:] == before[9:]
    assert after[8] != before[8]
    assert owner.legacy.last_segment_v2() is None
    owner.runner.close()


@pytest.mark.parametrize("bad", ["list", "row_list", "short", "long", "site_bool", "site_missing",
                                "edge_bool", "edge_negative", "edge_large", "edge_float", "child_none",
                                "child_v2", "duplicate"])
def test_malformed_later_edge_is_transactional_and_leaves_no_publication(bad):
    owner = Owner()
    parent, child = _pair(owner)
    good = (0, 0, child)
    rows = {
        "list": [good], "row_list": (list(good),), "short": ((0, child),),
        "long": ((0, 1, child, child),), "site_bool": ((True, 1, child),),
        "site_missing": ((1, 1, child),), "edge_bool": ((0, True, child),),
        "edge_negative": ((0, -1, child),), "edge_large": ((0, 4096, child),),
        "edge_float": ((0, 1.0, child),), "child_none": ((0, 1, None),),
        "child_v2": ((0, 1, owner.spec(2, version=2)),), "duplicate": (good,),
    }[bad]
    if type(rows) is tuple:
        rows = (good,) + rows
    owner.warm(parent)
    before = owner.snapshot()
    with pytest.raises((TypeError, ValueError)):
        owner.runner.publish_code_v3(parent, rows)
    assert owner.snapshot() == before
    assert not owner.runner.is_code_published_v3(parent)
    assert owner.runner.is_code_published_v3(child)
    assert len(owner.runner.publish_code_v3(parent, (good,))) == 1
    owner.runner.close()


def test_container_and_integer_subclasses_cannot_run_python_during_admission():
    owner = Owner()
    parent, child = _pair(owner)
    observed = []

    class Rows(tuple):
        def __iter__(self):
            observed.append("iterate")
            return super().__iter__()

    class Index(int):
        def __index__(self):
            observed.append("index")
            return 0

    for rows in (Rows(((0, 0, child),)), (Rows((0, 0, child)),), ((Index(0), 0, child),),
                 ((0, Index(0), child),)):
        before = owner.snapshot()
        with pytest.raises(TypeError):
            owner.runner.publish_code_v3(parent, rows)
        assert owner.snapshot() == before
    assert observed == [] and not owner.runner.is_code_published_v3(parent)
    owner.runner.close()


@pytest.mark.parametrize("method", ["publish_code_v3", "revoke_code_v3", "is_code_published_v3"])
def test_none_spec_rejects_before_any_owner_effect(method):
    owner = Owner()
    before = owner.snapshot()
    with pytest.raises(TypeError):
        getattr(owner.runner, method)(None)
    assert owner.snapshot() == before
    owner.runner.close()


def test_child_requires_exact_live_publication_by_the_same_owner():
    owner, foreign = Owner(), Owner()
    parent, child = _pair(owner)
    clone = _clone(child)
    foreign_child = foreign.spec(1)
    foreign.runner.publish_code_v3(foreign_child)
    assert not owner.runner.is_code_published_v3(clone)
    for target in (clone, foreign_child, parent):
        before = owner.snapshot()
        with pytest.raises(ValueError):
            owner.runner.publish_code_v3(parent, ((0, 0, target),))
        assert owner.snapshot() == before
        assert not owner.runner.is_code_published_v3(parent)
    assert len(owner.runner.publish_code_v3(parent, ((0, 0, child),))) == 1
    owner.runner.close()
    foreign.runner.close()


def test_child_signature_can_differ_from_its_parent_callback_policy_signature():
    owner = Owner()
    child = owner.spec(1, input_cells=2, output_cells=3)
    parent = owner.spec(0, sites=1)
    owner.runner.publish_code_v3(child)
    # The policy may prepare two inputs and reduce three outputs back to one.
    # Native publication binds the static edge; it does not execute that policy.
    assert len(owner.runner.publish_code_v3(parent, ((0, 0, child),))) == 1
    owner.runner.close()


def test_child_revocation_permanently_stales_old_parent_edges_until_parent_republication():
    owner = Owner()
    parent, child = _pair(owner)
    rows = ((0, 0, child),)
    old, = owner.runner.publish_code_v3(parent, rows)
    owner.runner.revoke_code_v3(child)
    assert owner.runner.is_code_published_v3(parent)
    for republish_child in (False, True):
        if republish_child:
            owner.runner.publish_code_v3(child)
        before = owner.snapshot()
        with pytest.raises(ValueError):
            owner.runner.publish_code_v3(parent, rows)
        assert owner.snapshot() == before
    owner.runner.revoke_code_v3(parent)
    fresh, = owner.runner.publish_code_v3(parent, rows)
    assert fresh is not old
    owner.runner.close()


def test_transitively_stale_child_generation_cannot_be_hidden_behind_a_live_direct_child():
    owner = Owner()
    leaf = owner.spec(2)
    middle = owner.spec(1, sites=1)
    parent = owner.spec(0, sites=1)
    owner.runner.publish_code_v3(leaf)
    owner.runner.publish_code_v3(middle, ((0, 0, leaf),))
    rows = ((0, 0, middle),)
    owner.runner.publish_code_v3(parent, rows)
    owner.runner.revoke_code_v3(leaf)
    owner.runner.publish_code_v3(leaf)
    before = owner.snapshot()
    with pytest.raises(ValueError):
        owner.runner.publish_code_v3(parent, rows)
    assert owner.snapshot() == before
    assert owner.runner.is_code_published_v3(parent)
    assert owner.runner.is_code_published_v3(middle)
    owner.runner.close()


def test_revoked_registration_cannot_reenter_its_own_old_dependency_generation():
    owner = Owner()
    first, second = owner.spec(0, sites=1), owner.spec(1, sites=1)
    owner.runner.publish_code_v3(first)
    owner.runner.publish_code_v3(second, ((0, 0, first),))
    owner.runner.revoke_code_v3(first)
    before = owner.snapshot()
    with pytest.raises(ValueError):
        owner.runner.publish_code_v3(first, ((0, 0, second),))
    assert owner.snapshot() == before
    assert not owner.runner.is_code_published_v3(first)
    assert owner.runner.is_code_published_v3(second)
    # Failed recapture left the first registration and its row allowance free.
    assert owner.runner.publish_code_v3(first) == ()
    owner.runner.close()


def test_source_seals_are_checked_before_publication_or_dependency_capture():
    owner = Owner()
    parent, child = _pair(owner)
    for target in (parent, child):
        owner.ram[target.code_base] ^= 1
        before = owner.snapshot()
        with pytest.raises(ValueError):
            owner.runner.publish_code_v3(parent, ((0, 0, child),))
        assert owner.snapshot() == before
        assert not owner.runner.is_code_published_v3(parent)
        owner.ram[target.code_base] ^= 1
    assert len(owner.runner.publish_code_v3(parent, ((0, 0, child),))) == 1
    owner.runner.close()


def test_ancestor_control_overlap_is_rejected_but_siblings_may_reuse_an_inactive_slice():
    owner = Owner()
    leaf = owner.spec(2)
    middle = owner.spec(1, sites=1)
    owner.runner.publish_code_v3(leaf)
    owner.runner.publish_code_v3(middle, ((0, 0, leaf),))
    for overlap in (middle.stack_base, leaf.stack_base):
        parent = owner.spec(0, sites=1, stack_base=overlap)
        before = owner.snapshot()
        with pytest.raises(ValueError):
            owner.runner.publish_code_v3(parent, ((0, 0, middle),))
        assert owner.snapshot() == before
        assert not owner.runner.is_code_published_v3(parent)
    sibling = owner.spec(3, stack_base=leaf.stack_base)
    owner.runner.publish_code_v3(sibling)
    parent = owner.spec(0, sites=1)
    assert len(owner.runner.publish_code_v3(parent, ((0, 0, leaf), (0, 1, sibling)))) == 2
    owner.runner.close()


def test_dependency_depth_eight_is_accepted_and_ninth_publication_is_transactional():
    owner = Owner()
    child = owner.spec(0)
    owner.runner.publish_code_v3(child)
    for slot in range(1, 8):
        parent = owner.spec(slot, sites=1)
        assert len(owner.runner.publish_code_v3(parent, ((0, 0, child),))) == 1
        child = parent
    ninth = owner.spec(8, sites=1)
    before = owner.snapshot()
    with pytest.raises(ValueError):
        owner.runner.publish_code_v3(ninth, ((0, 0, child),))
    assert owner.snapshot() == before and not owner.runner.is_code_published_v3(ninth)
    assert owner.runner.is_code_published_v3(child)
    owner.runner.close()


def test_row_capacity_is_per_routine_and_owner_wide_and_revocation_reclaims_it():
    owner = Owner()
    child = owner.spec(0)
    owner.runner.publish_code_v3(child)
    rows = tuple((0, index, child) for index in range(4096))
    parents = [owner.spec(slot, sites=2) for slot in range(1, 18)]
    before = owner.snapshot()
    with pytest.raises(ValueError):
        owner.runner.publish_code_v3(parents[0], rows + ((1, 0, child),))
    assert owner.snapshot() == before
    for parent in parents[:16]:
        assert len(owner.runner.publish_code_v3(parent, rows)) == 4096
    before = owner.snapshot()
    with pytest.raises(ValueError):
        owner.runner.publish_code_v3(parents[16], ((0, 0, child),))
    assert owner.snapshot() == before and not owner.runner.is_code_published_v3(parents[16])
    owner.runner.revoke_code_v3(parents[0])
    assert len(owner.runner.publish_code_v3(parents[16], rows)) == 4096
    owner.runner.close()


@pytest.mark.parametrize("exhausted_version", [2, 3])
def test_publication_count_is_shared_with_legacy_and_reclaimed_across_views(exhausted_version):
    owner = Owner()
    v2 = [owner.spec(version=2) for _ in range(32)]
    v3 = [owner.spec() for _ in range(32)]
    for old, new in zip(v2, v3):
        owner.legacy.publish_code_v2(old)
        owner.runner.publish_code_v3(new)
    extra = owner.spec(version=exhausted_version)
    publish = getattr(owner.legacy if exhausted_version == 2 else owner.runner,
                      f"publish_code_v{exhausted_version}")
    before = owner.snapshot()
    with pytest.raises(ValueError):
        publish(extra)
    assert owner.snapshot() == before
    if exhausted_version == 2:
        owner.runner.revoke_code_v3(v3[0])
    else:
        owner.legacy.revoke_code_v2(v2[0])
    publish(extra)
    owner.runner.close()


@pytest.mark.parametrize("exhausted_version", [2, 3])
def test_code_bytes_are_shared_between_old_and_new_publications(exhausted_version):
    owner = Owner(ram_size=2 << 20)
    v2 = [owner.spec(version=2, code_size=1 << 20) for _ in range(8)]
    v3 = [owner.spec(code_size=1 << 20) for _ in range(8)]
    for old, new in zip(v2, v3):
        owner.legacy.publish_code_v2(old)
        owner.runner.publish_code_v3(new)
    extra = owner.spec(version=exhausted_version, code_size=1 << 20)
    publish = getattr(owner.legacy if exhausted_version == 2 else owner.runner,
                      f"publish_code_v{exhausted_version}")
    with pytest.raises(ValueError):
        publish(extra)
    if exhausted_version == 2:
        owner.runner.revoke_code_v3(v3[0])
    else:
        owner.legacy.revoke_code_v2(v2[0])
    publish(extra)
    owner.runner.close()


def test_parked_legacy_frame_excludes_all_nested_publication_routes():
    owner = Owner()
    parent, child = _pair(owner)
    owner.runner.publish_code_v3(parent, ((0, 0, child),))
    legacy = owner.spec(3, sites=1, version=2)
    owner.legacy.publish_code_v2(legacy)
    first = owner.legacy.begin_v2(legacy, (7,), (), 100)
    assert first.exit_kind == "callback_request"
    before = owner.snapshot()
    for action in (
        lambda: owner.runner.publish_code_v3(parent, ((0, 0, child),)),
        lambda: owner.runner.revoke_code_v3(parent),
        lambda: owner.runner.is_code_published_v3(parent),
        owner.runner.close,
    ):
        with pytest.raises(RuntimeError):
            action()
        assert owner.snapshot() == before
    assert owner.legacy.last_segment_v2().segment_id == first.segment_id
    final = owner.legacy.resume_callback(first.token, (5,))
    assert final.exit_kind == "returned" and final.outputs == (5,)
    assert owner.runner.is_code_published_v3(parent)
    owner.runner.close()


def test_retained_edge_handles_do_not_keep_the_owner_or_control_pin_alive():
    owner = Owner()
    parent, child = _pair(owner)
    edge, = owner.runner.publish_code_v3(parent, ((0, 0, child),))
    owner.runner = owner.legacy = None
    gc.collect()
    owner.control.extend(b"x")
    owner.state.attach_mem(bytearray(4096), 4096)
    assert edge is not None


@pytest.mark.parametrize("close_view", ["runner", "legacy"])
def test_closing_either_view_revokes_nested_publication_and_releases_all_pins(close_view):
    owner = Owner()
    parent, child = _pair(owner)
    edge, = owner.runner.publish_code_v3(parent, ((0, 0, child),))
    getattr(owner, close_view).close()
    for action in (
        lambda: owner.runner.publish_code_v3(parent, ((0, 0, child),)),
        lambda: owner.runner.is_code_published_v3(parent),
        lambda: owner.runner.revoke_code_v3(parent),
    ):
        with pytest.raises(RuntimeError):
            action()
    owner.control.extend(b"x")
    owner.state.attach_mem(bytearray(4096), 4096)
    assert edge is not None
