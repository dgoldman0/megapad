"""One native resource owner behind separate nested and legacy transport views."""

from __future__ import annotations

import gc

import pytest
import _mp64_accel as native

from asm import assemble


CODE_BASE = 0x100
CONTROL_BASE, CONTROL_SIZE = 0x200000, 128
CALLBACK = """
    ldi64 r12, stub
call:
    call.l r12
    ret.l
stub:
    ret.l
"""


class Owner:
    def __init__(self, *, callback=False, code_size=16):
        labels = {}
        raw = bytes(assemble(CALLBACK if callback else "ret.l", base_addr=CODE_BASE,
                             labels_out=labels))
        code_size = max(code_size, (len(raw) + 15) & ~15)
        self.image = raw + b"\x01" * (code_size - len(raw))
        self.ram = bytearray(max(4096, CODE_BASE + code_size))
        self.ram[CODE_BASE:CODE_BASE + code_size] = self.image
        self.control = bytearray(CONTROL_SIZE)
        self.state = native.CPUState()
        self.state.attach_mem(self.ram, len(self.ram))
        self.root = native.RoutineRunnerV3(self.state, CONTROL_BASE, self.control)
        self.legacy = self.root.legacy_v2()
        self.callbacks = ((labels["call"] - CODE_BASE, labels["stub"] - CODE_BASE, 0, 1, 1),) if callback else ()
        self.spec = self.new_spec()
        self.v1 = native.RoutineSpecV1(CODE_BASE, len(self.image), 0, 1, 1,
                                      CONTROL_BASE, CONTROL_SIZE, 100)

    def new_spec(self):
        return native.RoutineSpecV2(CODE_BASE, self.image, 0, 1, 1,
                                   CONTROL_BASE, CONTROL_SIZE, 100, self.callbacks)

    def begin(self, argument=7, *, spec=None, view=None):
        runner = self.legacy if view is None else view
        return runner.begin_v2(self.spec if spec is None else spec, (argument,), (), 100)

    def snapshot(self):
        return (tuple(self.state.get_reg(index) for index in range(32)),
                self.state.flags_pack(), self.state.psel, self.state.xsel,
                self.state.spsel, self.state.cycle_count, self.state.icache_hits,
                self.state.icache_misses, bytes(self.ram), bytes(self.control))


def test_owner_shell_withholds_nested_capability_and_legacy_entry_routes():
    owner = Owner()
    assert not hasattr(native, "HYBRID_NESTED_ROUTINE_ABI_VERSION")
    assert type(owner.root) is native.RoutineRunnerV3
    assert not isinstance(owner.root, (native.RoutineRunnerV1, native.RoutineRunnerV2))
    assert isinstance(owner.legacy, native.RoutineRunnerV1)
    assert type(owner.legacy) is native.RoutineRunnerV2
    assert owner.root.legacy_v2() is owner.legacy
    assert owner.root.control_base == owner.legacy.control_base == CONTROL_BASE
    assert owner.root.control_size == owner.legacy.control_size == CONTROL_SIZE
    for name in ("run", "publish_code", "begin_v2", "resume_callback", "cancel_invocation",
                 "publish_code_v2", "begin_root_v3", "begin_child_v3", "publish_code_v3"):
        assert not hasattr(owner.root, name)
    owner.root.close()


@pytest.mark.parametrize("version", (1, 2))
def test_old_standalone_constructors_and_public_inheritance_stay_unchanged(version):
    state, ram, control = native.CPUState(), bytearray(4096), bytearray(CONTROL_SIZE)
    state.attach_mem(ram, len(ram))
    runner = getattr(native, f"RoutineRunnerV{version}")(state, CONTROL_BASE, control)
    assert isinstance(runner, native.RoutineRunnerV1)
    assert isinstance(runner, native.RoutineRunnerV2) is (version == 2)
    if version == 1:
        assert not hasattr(runner, "begin_v2")
    assert not hasattr(runner, "legacy_v2")
    runner.close()
    runner.close()
    control.extend(b"x")
    state.attach_mem(bytearray(4096), 4096)


@pytest.mark.parametrize("version", (1, 2, 3))
def test_separately_constructed_runner_cannot_acquire_another_pin(version):
    owner = Owner()
    before = owner.snapshot()
    with pytest.raises(RuntimeError, match="pins"):
        getattr(native, f"RoutineRunnerV{version}")(owner.state, CONTROL_BASE, bytearray(CONTROL_SIZE))
    assert owner.snapshot() == before
    owner.root.close()
    replacement = getattr(native, f"RoutineRunnerV{version}")(
        owner.state, CONTROL_BASE, owner.control)
    replacement.close()


def test_cached_views_share_publication_revoke_and_v2_sequence_space():
    owner = Owner()
    other = owner.root.legacy_v2()
    owner.legacy.publish_code_v2(owner.spec)
    assert other.is_code_published_v2(owner.spec)
    first = owner.begin(view=other)
    assert first.exit_kind == "returned" and first.outputs == (7,)
    assert (first.segment_id, first.invocation_id, first.instructions) == (1, 1, 1)
    owner.legacy.publish_code(owner.v1)
    v1 = native.RoutineRunnerV1.run(other, owner.v1, (9,), (), 100)
    assert v1.exit_kind == "returned" and v1.outputs == (9,)
    assert other.last_segment_v2().segment_id == first.segment_id
    other.revoke_code_v2(owner.spec)
    assert not owner.legacy.is_code_published_v2(owner.spec)
    with pytest.raises(ValueError):
        owner.begin()
    other.publish_code_v2(owner.spec)
    second = owner.begin(argument=11)
    assert second.outputs == (11,)
    assert (second.segment_id, second.invocation_id) == (2, 2)
    assert owner.legacy.last_segment_v2().segment_id == other.last_segment_v2().segment_id
    owner.root.close()


def test_publication_count_is_owner_wide_and_revoke_reclaims_one_slot():
    owner = Owner()
    views = (owner.legacy, owner.root.legacy_v2())
    specs = [owner.new_spec() for _ in range(65)]
    for index, spec in enumerate(specs[:64]):
        views[index % 2].publish_code_v2(spec)
    # Re-publishing one exact identity is idempotent and cannot consume a slot.
    views[1].publish_code_v2(specs[0])
    before = owner.snapshot()
    with pytest.raises(ValueError, match="publication count|aggregate code"):
        views[0].publish_code_v2(specs[64])
    assert owner.snapshot() == before
    views[1].revoke_code_v2(specs[0])
    views[0].publish_code_v2(specs[64])
    assert views[1].is_code_published_v2(specs[64])
    assert not views[0].is_code_published_v2(specs[0])
    owner.root.close()


def test_aggregate_sealed_bytes_are_not_replenished_by_legacy_access():
    owner = Owner(code_size=1024 * 1024)
    specs = [owner.new_spec() for _ in range(17)]
    for spec in specs[:16]:
        owner.root.legacy_v2().publish_code_v2(spec)
    with pytest.raises(ValueError, match="aggregate code"):
        owner.legacy.publish_code_v2(specs[16])
    owner.legacy.revoke_code_v2(specs[0])
    owner.root.legacy_v2().publish_code_v2(specs[16])
    owner.root.close()


@pytest.mark.parametrize("close_view", ("root", "legacy", "v1_base"))
def test_explicit_close_invalidates_every_view_and_releases_pins(close_view):
    owner = Owner()
    owner.legacy.publish_code_v2(owner.spec)
    result = owner.begin()
    if close_view == "v1_base":
        native.RoutineRunnerV1.close(owner.legacy)
    else:
        getattr(owner, close_view).close()
    assert owner.root.legacy_v2() is owner.legacy
    for action in (
        lambda: owner.legacy.publish_code_v2(owner.spec),
        owner.begin,
        lambda: owner.legacy.publish_code(owner.v1),
        lambda: owner.legacy.run(owner.v1, (7,), (), 100),
        lambda: owner.legacy.is_code_published_v2(owner.spec),
    ):
        with pytest.raises(RuntimeError, match="closed"):
            action()
    assert owner.legacy.last_segment_v2().segment_id == result.segment_id
    assert owner.legacy.cancel_invocation() is None
    owner.root.close()
    owner.legacy.close()
    owner.control.extend(b"x")
    owner.state.attach_mem(bytearray(4096), 4096)


def test_root_collection_does_not_release_retained_legacy_authority():
    owner = Owner(callback=True)
    owner.legacy.publish_code_v2(owner.spec)
    first = owner.begin()
    assert first.exit_kind == "callback_request"
    owner.root = None
    gc.collect()
    with pytest.raises(BufferError):
        owner.control.extend(b"x")
    with pytest.raises(RuntimeError):
        owner.state.attach_mem(bytearray(4096), 4096)
    final = owner.legacy.resume_callback(first.token, (3,))
    assert final.exit_kind == "returned" and final.outputs == (3,)
    owner.legacy.close()
    owner.control.extend(b"x")
    owner.state.attach_mem(bytearray(4096), 4096)


def test_legacy_wrapper_collection_does_not_release_retained_root_owner():
    owner = Owner()
    owner.legacy.publish_code_v2(owner.spec)
    owner.legacy = None
    gc.collect()
    with pytest.raises(BufferError):
        owner.control.extend(b"x")
    view = owner.root.legacy_v2()
    assert view.is_code_published_v2(owner.spec)
    assert owner.begin(view=view).outputs == (7,)
    owner.root.close()
    owner.control.extend(b"x")


def test_last_view_collection_releases_owner_even_with_retained_callback_token():
    owner = Owner(callback=True)
    owner.legacy.publish_code_v2(owner.spec)
    first = owner.begin()
    owner.legacy = owner.root = None
    gc.collect()
    # Token authority is weak and cannot keep the CPU reservation/control pin alive.
    owner.control.extend(b"x")
    owner.state.set_reg(4, 91)
    owner.state.attach_mem(bytearray(4096), 4096)
    assert first.token is not None


def test_parked_v2_retains_its_own_cancel_close_and_blocks_unrelated_views():
    owner = Owner(callback=True)
    owner.legacy.publish_code_v2(owner.spec)
    first = owner.begin()
    assert first.exit_kind == "callback_request"
    assert first.instructions == 2
    before = owner.snapshot()
    for action in (
        owner.root.close,
        lambda: owner.legacy.run(owner.v1, (99,), (), 100),
        lambda: owner.legacy.publish_code(owner.v1),
        owner.begin,
        lambda: owner.state.set_reg(4, 99),
    ):
        with pytest.raises(RuntimeError):
            action()
        assert owner.snapshot() == before
    assert owner.root.legacy_v2() is owner.legacy
    final = owner.legacy.resume_callback(first.token, (3,))
    assert final.exit_kind == "returned" and final.outputs == (3,)
    assert final.instructions == 2 and final.segment_id == first.segment_id + 1
    second = owner.begin()
    owner.legacy.close()
    with pytest.raises(RuntimeError, match="closed"):
        owner.legacy.resume_callback(second.token, (0,))
    owner.root.close()
    owner.control.extend(b"x")
