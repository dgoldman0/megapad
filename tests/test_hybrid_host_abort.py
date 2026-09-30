"""A raw private callback ABORT-shaped error is still a host escape outside."""

import pytest

pytest.importorskip("_mp64_accel")

from asm import assemble
from hybrid.runtime import HybridRuntime
from shared.hybrid_abi import CallbackExportV2, CallbackSiteV2, RoutineImageV2
from shared.hybrid_nested import CallbackExportV4, CallbackSiteV4, RoutineImageV4
from simulator.errors import ForthAbort
from simulator.ir import Call, Return


@pytest.mark.parametrize("executor", ("python", "native"))
@pytest.mark.parametrize("version", (2, 4))
@pytest.mark.parametrize("entry", ("execute", "evaluate", "resume"))
def test_public_machine_word_preserves_raw_callback_abort(executor, version, entry, monkeypatch):
    if executor == "native":
        pytest.importorskip("_megaforth_native")
    owner = HybridRuntime.create(executor=executor,
                                 geometry={"bank0_size": 65536, "external_size": 65536})
    try:
        export_type, site_type, image_type = (
            (CallbackExportV2, CallbackSiteV2, RoutineImageV2) if version == 2
            else (CallbackExportV4, CallbackSiteV4, RoutineImageV4)
        )
        export = export_type(export_id=0, name="ABS", input_cells=1, output_cells=1,
                             max_semantic_steps=1, effect="integer_leaf")
        program = "mov r12, r3\nafter_pc:\naddi r12, 0\ncall:\ncall.l r12\nret.l\nstub:\nret.l"
        labels = {}
        assemble(program, labels_out=labels)
        program = program.replace("addi r12, 0", f"addi r12, {labels['stub'] - labels['after_pc']}")
        extra = {} if version == 2 else {"routine_id": 0, "max_callback_requests": 1}
        image = image_type(name="MACHINE", code=bytes(assemble(program)), entry_offset=0,
                           input_cells=1, output_cells=1, buffers=(), return_stack_cells=16,
                           max_instructions=10, callbacks=(site_type(call_offset=labels["call"],
                              stub_offset=labels["stub"], export=export),), **extra)
        word = getattr(owner, f"register_routine_v{version}")(image)
        context = owner.semantic.main_context
        context.data.push(7)
        context.returns.push(19)
        error = ForthAbort("host accounting failed")
        account = owner.semantic._account_semantic_step
        def fail_in_private_callback():
            account()
            if owner.semantic._callback_exports._active_context is not None:
                raise error
        monkeypatch.setattr(owner.semantic, "_account_semantic_step", fail_in_private_callback)
        if entry == "resume":
            noop = owner.semantic.define_primitive("NOOP", lambda context: None)
            outer = owner.semantic.define_colon("RUN", (Call(noop.xt), Call(word.xt), Return()))
            report = owner.run(outer.xt, quantum_steps=1)
            invoke = lambda: owner.resume_yielded(report.semantic_result.suspension)
        elif entry == "evaluate":
            invoke = lambda: owner.evaluate("MACHINE")
        else:
            invoke = lambda: owner.execute(word.xt)
        with pytest.raises(ForthAbort) as caught:
            invoke()
        assert caught.value is error
        assert error.origin_context is None
        assert context.data.snapshot() == (7,)
        assert context.returns.snapshot() == (19,)
        assert context.reusable
        assert owner.semantic._private_host_abort._error is None
        assert owner.callback_semantic_steps == 1
        assert (owner.machine_instructions, owner.callback_requests, owner.transitions) == (3, 1, 1)
    finally:
        owner.close()
