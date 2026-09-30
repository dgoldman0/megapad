"""Failed host publication rolls back the dictionary and its guest index."""

from __future__ import annotations

import pytest


pytest.importorskip("_mp64_accel")

from asm import assemble  # noqa: E402
from hybrid.runtime import HybridExecutionError, HybridRuntime  # noqa: E402
from shared.hybrid_abi import RoutineImageV1  # noqa: E402
from simulator.memory import EXTERNAL_BASE  # noqa: E402
from simulator.platform import create_one_core_address_space  # noqa: E402


INDEX_SLOTS = 2048
INDEX_BYTES = INDEX_SLOTS * 16


def _image(name):
    return RoutineImageV1(
        name=name, code=bytes(assemble("ret.l")), entry_offset=0,
        input_cells=0, output_cells=0, buffers=(), max_instructions=10,
        return_stack_cells=8,
    )


@pytest.fixture(params=("python", "native"))
def owner(request):
    if request.param == "native":
        pytest.importorskip("_megaforth_native")
    memory = create_one_core_address_space(
        bank0_size=65536, external_size=65536, dense_backing=True,
    )
    runtime = HybridRuntime.create(executor=request.param, memory=memory)
    try:
        yield runtime
    finally:
        runtime.close()


class _FailPublication:
    def __init__(self, runner, error):
        self.runner = runner
        self.error = error

    def publish_code(self, spec):
        self.runner.publish_code(spec)
        raise self.error

    def __getattr__(self, name):
        return getattr(self.runner, name)


@pytest.mark.parametrize("failure_point", ("code_publication", "index_publication"))
def test_failed_registration_restores_live_dictionary_and_bound_index(
    owner, monkeypatch, failure_point,
):
    semantic = owner.semantic
    dictionary = semantic.dictionary
    original = owner.register_routine_v1(_image("H-EXISTING"))
    original_lease = owner.declaration_for(original).allocation_lease
    assert semantic.configure_dictionary_index(EXTERNAL_BASE, INDEX_SLOTS) == 0
    index = semantic.dictionary_index
    before_here, before_latest = dictionary.here, dictionary.latest
    before_words = dictionary.words
    before_index = index.state
    before_bytes = semantic.memory.read_bytes(EXTERNAL_BASE, INDEX_BYTES)
    before_control_used = owner._control_used
    failed_words = []
    failure = RuntimeError(f"injected {failure_point} failure")

    with monkeypatch.context() as patch:
        if failure_point == "code_publication":
            patch.setattr(owner, "_runner", _FailPublication(owner._runner, failure))
        else:
            publish = type(index).publish

            def fail_after_index_publication(current, word):
                publish(current, word)
                if current is index:
                    failed_words.append((word, dictionary.acquire_body_lease(word)))
                    raise failure

            patch.setattr(type(index), "publish", fail_after_index_publication)
        with pytest.raises(RuntimeError) as caught:
            owner.register_routine_v1(_image("H-NEW"))

    assert caught.value is failure
    assert (dictionary.here, dictionary.latest) == (before_here, before_latest)
    assert dictionary.words == before_words
    assert semantic.find("H-NEW") is None
    assert index.state == before_index
    assert semantic.memory.read_bytes(EXTERNAL_BASE, INDEX_BYTES) == before_bytes
    assert dictionary.is_body_lease_live(original_lease)
    for word, lease in failed_words:
        assert not dictionary.is_body_lease_live(lease)
        with pytest.raises(KeyError):
            dictionary.resolve(word.xt)
    assert owner._control_used == before_control_used
    assert owner.transitions == owner.machine_instructions == 0

    # A failed publication consumes neither the host name nor its control
    # allocation, and does not invalidate an unrelated live registration.
    assert owner.execute_xt(original.xt).machine_instructions == 1
    replacement = owner.register_routine_v1(_image("H-NEW"))
    assert owner.execute_xt(replacement.xt).machine_instructions == 1
    assert index.state.count == before_index.count + 1


def test_cleanup_failure_preserves_original_error_and_disables_machine_entry(owner, monkeypatch):
    original = owner.register_routine_v1(_image("H-EXISTING"))
    failure = RuntimeError("injected code publication failure")
    cleanup_failure = RuntimeError("injected index rebuild failure")
    index = owner.semantic.dictionary_index
    rebuild = type(index).rebuild

    def fail_rebuild(current):
        if current is index:
            raise cleanup_failure
        return rebuild(current)

    with monkeypatch.context() as patch:
        patch.setattr(owner, "_runner", _FailPublication(owner._runner, failure))
        patch.setattr(type(index), "rebuild", fail_rebuild)
        with pytest.raises(RuntimeError) as caught:
            owner.register_routine_v1(_image("H-NEW"))

    assert caught.value is failure
    assert owner.semantic.find("H-NEW") is None
    with pytest.raises(HybridExecutionError, match="registration_cleanup"):
        owner.execute_xt(original.xt)
    with pytest.raises(HybridExecutionError, match="registration_cleanup"):
        owner.semantic.execute(original.xt)
    assert owner.transitions == owner.machine_instructions == 0
