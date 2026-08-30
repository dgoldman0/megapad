"""Focused compiler and explicit-dispatch tests for the hosted runtime."""

from __future__ import annotations

import pytest

from shared.cells import CELL_BYTES, MASK64, TRUE
from simulator.errors import ExecutionError, SourceError, StepBudgetExceeded
from simulator.ir import (
    Branch,
    BranchZero,
    InstallDoes,
    Loop,
    QuestionDo,
    RestoreDataStackPointer,
    RestoreReturnStackPointer,
)
from simulator.memory import AddressClass, SparseAddressSpace
from simulator.platform import create_one_core_address_space
from simulator.runtime import (
    ColonDefinition,
    ConstantDefinition,
    CreatedDefinition,
    DirectiveKind,
    DoesBodyRef,
    MegaForthRuntime,
)
from simulator.stacks import ReturnStackShapeError, StackPointerError


def test_runtime_owns_a_default_address_space_or_uses_the_injected_one() -> None:
    first = MegaForthRuntime()
    second = MegaForthRuntime()
    injected = create_one_core_address_space(bank0_size=0x4000)

    assert isinstance(first.memory, SparseAddressSpace)
    assert first.memory is not second.memory
    assert MegaForthRuntime(memory=injected).memory is injected

    with pytest.raises(TypeError, match="SparseAddressSpace"):
        MegaForthRuntime(memory=object())  # type: ignore[arg-type]


def test_main_context_uses_the_exact_cell_aligned_bank0_stack_halves() -> None:
    runtime = MegaForthRuntime()
    bank0 = next(
        region
        for region in runtime.memory.regions
        if region.kind is AddressClass.BANK0
    )

    assert runtime.main_context.data.pointer == bank0.base + bank0.size // 2
    assert runtime.main_context.returns.pointer == bank0.limit
    assert runtime.main_context.data.pointer % CELL_BYTES == 0
    assert runtime.main_context.returns.pointer % CELL_BYTES == 0


def test_compiled_stack_pointer_restores_are_explicit_ir_operations() -> None:
    runtime = MegaForthRuntime()
    runtime.define_directive("DSP-RESTORE", DirectiveKind.SP_STORE)
    runtime.define_directive("RSP-RESTORE", DirectiveKind.RP_STORE)
    runtime.evaluate(
        b": RESTORE-DATA DSP-RESTORE DROP 77 ; "
        b": ESCAPE RSP-RESTORE 42 ; "
        b": CALL-ESCAPE RP@ ESCAPE 99 ;"
    )

    restore_data = runtime.find("RESTORE-DATA")
    escape = runtime.find("ESCAPE")
    assert restore_data is not None
    assert escape is not None
    assert isinstance(restore_data.implementation, ColonDefinition)
    assert isinstance(escape.implementation, ColonDefinition)
    assert any(
        isinstance(operation, RestoreDataStackPointer)
        for operation in restore_data.implementation.operations
    )
    assert any(
        isinstance(operation, RestoreReturnStackPointer)
        for operation in escape.implementation.operations
    )

    context = runtime.main_context
    context.data.push(11)
    saved_data_pointer = context.data.pointer
    context.data.push(22)
    context.data.push(saved_data_pointer)
    runtime.execute("RESTORE-DATA", context=context)
    assert context.data.snapshot() == (77,)

    context.data.clear()
    runtime.execute("CALL-ESCAPE", context=context)
    assert context.data.snapshot() == (42,)
    assert context.returns.snapshot() == ()


def test_out_of_bounds_rp_restore_preserves_argument_and_context_usability() -> None:
    runtime = MegaForthRuntime()
    runtime.define_directive("RSP-RESTORE", DirectiveKind.RP_STORE)
    runtime.evaluate(b": BAD-RSP RSP-RESTORE ;")
    context = runtime.main_context
    invalid_pointer = 0
    context.data.push(invalid_pointer)

    with pytest.raises(StackPointerError, match="caller-owned stack span"):
        runtime.execute("BAD-RSP", context=context)

    assert context.data.snapshot() == (invalid_pointer,)
    assert context.returns.snapshot() == ()
    assert context.reusable


def test_in_span_rp_restore_does_not_require_a_prior_rp_fetch() -> None:
    runtime = MegaForthRuntime()
    runtime.define_directive("RSP-RESTORE", DirectiveKind.RP_STORE)
    runtime.evaluate(b": RAW-RSP RSP-RESTORE ;")
    context = runtime.main_context

    # Entering RAW-RSP places its root continuation in this slot.  RP! is a
    # raw machine-compatible restore and must not require an RP@ registration.
    root_pointer = context.returns.empty_pointer - CELL_BYTES
    context.data.push(root_pointer)
    runtime.execute("RAW-RSP", context=context)

    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert context.reusable


def test_created_word_pushes_its_body_in_every_dispatch_path() -> None:
    runtime = MegaForthRuntime()
    child = runtime.define_created("BUFFER")
    runtime.dictionary.allot(8)
    runtime.evaluate(b": COMPILED BUFFER ;")

    assert isinstance(child.implementation, CreatedDefinition)
    assert child.implementation.action is None

    direct = runtime.new_context()
    assert runtime.execute(child.xt, context=direct).semantic_steps == 1
    assert direct.data.snapshot() == (child.body_address,)

    compiled = runtime.new_context()
    assert runtime.execute("COMPILED", context=compiled).semantic_steps == 3
    assert compiled.data.snapshot() == (child.body_address,)

    dynamic = runtime.new_context()
    dynamic.data.push(child.xt)
    assert runtime.execute("EXECUTE", context=dynamic).semantic_steps == 2
    assert dynamic.data.snapshot() == (child.body_address,)


def test_two_created_children_keep_distinct_bodies_and_one_does_suffix() -> None:
    runtime = MegaForthRuntime()

    result = runtime.evaluate(
        b": BOX CREATE , DOES> @ ; 11 BOX FIRST 22 BOX SECOND"
    )
    box = runtime.find("BOX")
    first = runtime.find("FIRST")
    second = runtime.find("SECOND")
    assert box is not None
    assert first is not None
    assert second is not None
    assert [word.name for word in result.definitions] == [
        b"BOX",
        b"FIRST",
        b"SECOND",
    ]
    assert isinstance(box.implementation, ColonDefinition)
    assert isinstance(first.implementation, CreatedDefinition)
    assert isinstance(second.implementation, CreatedDefinition)

    install_index = next(
        index
        for index, operation in enumerate(box.implementation.operations)
        if isinstance(operation, InstallDoes)
    )
    expected_action = DoesBodyRef(box.xt, install_index + 2)
    assert first.implementation.action == expected_action
    assert second.implementation.action == expected_action
    assert first.body_address != second.body_address
    assert runtime.memory.read64(first.body_address) == 11
    assert runtime.memory.read64(second.body_address) == 22

    runtime.evaluate(b": PAIR FIRST SECOND ;")
    context = runtime.new_context()
    runtime.execute("PAIR", context=context)
    assert context.data.snapshot() == (11, 22)
    assert context.returns.snapshot() == ()


def test_does_suffix_keeps_absolute_if_and_loop_targets() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(
        b": CLASS CREATE , DOES> DUP @ "
        b"IF 3 1 DO I LOOP ELSE DROP 99 THEN ; "
        b"1 CLASS HOT 0 CLASS COLD"
    )

    defining_word = runtime.find("CLASS")
    hot = runtime.find("HOT")
    assert defining_word is not None
    assert hot is not None
    assert isinstance(defining_word.implementation, ColonDefinition)
    operations = defining_word.implementation.operations
    install_index = next(
        index for index, operation in enumerate(operations)
        if isinstance(operation, InstallDoes)
    )
    install = operations[install_index]
    assert isinstance(install, InstallDoes)
    assert install.entry_ip == install_index + 2
    assert all(
        operation.target >= install.entry_ip
        for operation in operations[install.entry_ip :]
        if isinstance(operation, (Branch, BranchZero, Loop, QuestionDo))
    )

    true_context = runtime.new_context()
    runtime.execute("HOT", context=true_context)
    assert true_context.data.snapshot() == (hot.body_address, 1, 2)
    assert true_context.returns.snapshot() == ()

    false_context = runtime.new_context()
    runtime.execute("COLD", context=false_context)
    assert false_context.data.snapshot() == (99,)
    assert false_context.returns.snapshot() == ()


def test_created_xt_and_does_action_survive_binding_shadowing() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": BOX CREATE , DOES> @ ; 7 BOX ITEM")
    old_box = runtime.find("BOX")
    old_item = runtime.find("ITEM")
    assert old_box is not None
    assert old_item is not None
    assert isinstance(old_item.implementation, CreatedDefinition)
    old_action = old_item.implementation.action
    assert isinstance(old_action, DoesBodyRef)

    runtime.evaluate(
        b"9 BOX item : BOX CREATE , DOES> @ 100 + ; 1 BOX LATER"
    )
    shadow_item = runtime.find("ITEM")
    new_box = runtime.find("BOX")
    later = runtime.find("LATER")
    assert shadow_item is not None
    assert new_box is not None
    assert later is not None
    assert shadow_item.xt != old_item.xt
    assert new_box.xt != old_box.xt
    assert isinstance(shadow_item.implementation, CreatedDefinition)
    assert isinstance(later.implementation, CreatedDefinition)
    assert old_item.implementation.action == old_action
    assert shadow_item.implementation.action == old_action
    later_action = later.implementation.action
    assert isinstance(later_action, DoesBodyRef)
    assert later_action.source_xt == new_box.xt

    context = runtime.new_context()
    runtime.execute(old_item.xt, context=context)
    runtime.execute("ITEM", context=context)
    runtime.execute("LATER", context=context)
    assert context.data.snapshot() == (7, 9, 101)
    assert context.returns.snapshot() == ()


def test_bracket_tick_compiles_exact_or_zero_xt_without_crossing_lines() -> None:
    runtime = MegaForthRuntime()
    drop = runtime.find("DROP")
    assert drop is not None

    runtime.evaluate(b": TOKENS ['] DROP ['] MISSING [']\n;")
    context = runtime.new_context()
    runtime.execute("TOKENS", context=context)

    assert context.data.snapshot() == (drop.xt, 0, 0)


def test_created_action_budget_failure_restores_the_callers_return_stack() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": STUCK CREATE DOES> 0 0 DO LOOP ; STUCK WAIT")
    context = runtime.new_context()
    context.returns.push(0xA5)

    with pytest.raises(StepBudgetExceeded):
        runtime.execute("WAIT", context=context, step_budget=10)

    assert context.returns.snapshot() == (0xA5,)


def test_uart_output_is_immutable_to_callers_and_can_be_drained() -> None:
    runtime = MegaForthRuntime()

    runtime.write_uart_bytes(b"abc")
    snapshot = runtime.uart_output
    runtime.write_uart_bytes(b"def")

    assert snapshot == b"abc"
    assert runtime.uart_output == b"abcdef"
    assert runtime.drain_uart_output() == b"abcdef"
    assert runtime.uart_output == b""
    with pytest.raises(TypeError, match="must be bytes"):
        runtime.write_uart_bytes(bytearray(b"x"))  # type: ignore[arg-type]


def test_constants_are_real_stable_xt_definitions_in_every_dispatch_path() -> None:
    runtime = MegaForthRuntime()
    original = runtime.define_constant("LIMIT", -1)
    assert isinstance(original.implementation, ConstantDefinition)
    assert original.implementation.value == MASK64

    runtime.evaluate(b": CAPTURED LIMIT ;")
    replacement = runtime.define_constant("limit", 2)

    direct = runtime.new_context()
    assert runtime.execute(original.xt, context=direct).semantic_steps == 1
    assert direct.data.snapshot() == (MASK64,)

    compiled = runtime.new_context()
    assert runtime.execute("CAPTURED", context=compiled).semantic_steps == 3
    assert compiled.data.snapshot() == (MASK64,)

    dynamic = runtime.new_context()
    dynamic.data.push(replacement.xt)
    assert runtime.execute("EXECUTE", context=dynamic).semantic_steps == 2
    assert dynamic.data.snapshot() == (2,)


def test_defining_primitives_use_nested_line_local_input_cursor_frames() -> None:
    runtime = MegaForthRuntime()
    nested_results = []

    def define_from_input(context) -> None:
        name = runtime.parse_required_input_word(b"MAKE-CONSTANT")
        runtime.define_constant(name, context.data.pop())

    def nested_definition(_context) -> None:
        nested_results.append(
            runtime.evaluate(
                b"11 MAKE-CONSTANT INNER",
                source_name="inner.f",
            )
        )
        name = runtime.parse_required_input_word("NESTED-DEFINITION")
        runtime.define_constant(name, 22)

    runtime.define_primitive("MAKE-CONSTANT", define_from_input)
    runtime.define_primitive("NESTED-DEFINITION", nested_definition)

    outer_result = runtime.evaluate(
        b"NESTED-DEFINITION OUTER",
        source_name="outer.f",
    )

    assert [
        [word.name for word in result.definitions] for result in nested_results
    ] == [[b"INNER"]]
    assert [result.semantic_steps for result in nested_results] == [1]
    assert outer_result.semantic_steps == 2
    assert [word.name for word in outer_result.definitions] == [b"OUTER"]

    context = runtime.new_context()
    runtime.evaluate(b"INNER OUTER", context=context)
    assert context.data.snapshot() == (11, 22)

    with pytest.raises(ExecutionError, match="active input line"):
        runtime.parse_required_input_word("OUTSIDE")


def test_nested_public_dispatch_inherits_the_callers_step_meter() -> None:
    runtime = MegaForthRuntime()
    nested_results = []

    def evaluate_inner(_context) -> None:
        nested_results.append(runtime.evaluate(b"1 DROP 2 DROP"))

    def execute_inner(_context) -> None:
        nested_results.append(runtime.execute("ONE-STEP"))

    def replace_budget(_context) -> None:
        runtime.evaluate(b"1 DROP", step_budget=10)

    runtime.define_primitive("EVALUATE-INNER", evaluate_inner)
    runtime.define_primitive("EXECUTE-INNER", execute_inner)
    runtime.define_primitive("REPLACE-BUDGET", replace_budget)
    runtime.evaluate(b": ONE-STEP 1 DROP ;")

    result = runtime.evaluate(b"EVALUATE-INNER EXECUTE-INNER", step_budget=8)
    assert result.semantic_steps == 8
    assert [nested.semantic_steps for nested in nested_results] == [2, 4]

    with pytest.raises(StepBudgetExceeded):
        runtime.evaluate(b"EVALUATE-INNER", step_budget=2)

    with pytest.raises(ValueError, match="cannot replace"):
        runtime.evaluate(b"REPLACE-BUDGET")


def test_required_input_word_does_not_continue_onto_the_next_line() -> None:
    runtime = MegaForthRuntime()

    def consume_name(_context) -> None:
        runtime.parse_required_input_word("DEFINER")

    runtime.define_primitive("DEFINER", consume_name)

    with pytest.raises(SourceError, match="requires a following word") as caught:
        runtime.evaluate(b"DEFINER\nNOT-A-NAME", source_name="line-local.f")

    assert caught.value.location.source_name == "line-local.f"
    assert caught.value.location.line == 1
    assert caught.value.location.column == len(b"DEFINER")


def test_question_do_patches_its_zero_trip_exit_and_preserves_outer_loop() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(
        b": ZERO-TRIP 4 4 ?DO 99 LOOP 7 ; "
        b": NESTED-ZERO 2 0 ?DO I 5 5 ?DO 99 LOOP I LOOP ;"
    )

    zero_trip = runtime.find("ZERO-TRIP")
    assert zero_trip is not None
    assert isinstance(zero_trip.implementation, ColonDefinition)
    operations = zero_trip.implementation.operations
    question_index = next(
        index for index, operation in enumerate(operations)
        if isinstance(operation, QuestionDo)
    )
    loop_index = next(
        index for index, operation in enumerate(operations)
        if isinstance(operation, Loop)
    )
    question_do = operations[question_index]
    assert isinstance(question_do, QuestionDo)
    assert question_do.target == loop_index + 1

    skipped = runtime.new_context()
    skipped_result = runtime.execute("ZERO-TRIP", context=skipped)
    assert skipped_result.semantic_steps == 5
    assert skipped.data.snapshot() == (7,)
    assert skipped.returns.snapshot() == ()

    nested = runtime.new_context()
    runtime.execute("NESTED-ZERO", context=nested)
    assert nested.data.snapshot() == (0, 0, 1, 1)
    assert nested.returns.snapshot() == ()


def test_colon_control_flow_and_wrapping_arithmetic_execute_from_source() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    result = runtime.evaluate(
        b": CLASSIFY DUP 0< IF DROP -1 EXIT THEN "
        b"DUP 0= IF DROP 0 ELSE DROP 1 THEN ; "
        b"-9 CLASSIFY 0 CLASSIFY 7 CLASSIFY -1 1 +",
        source_name="control.f",
        context=context,
    )

    assert [word.name for word in result.definitions] == [b"CLASSIFY"]
    assert context.data.snapshot() == (MASK64, 0, 1, 0)
    assert context.returns.snapshot() == ()


def test_compiled_calls_keep_old_xt_after_dictionary_shadowing() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(
        b": VALUE 1 ; : CALLER VALUE ; : value 2 ; CALLER VALUE",
        context=context,
    )

    assert context.data.snapshot() == (1, 2)
    assert runtime.find("VALUE") is runtime.find("value")


def test_execute_uses_numeric_virtual_xt_in_interpret_and_compiled_paths() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": ANSWER 42 ; : RUN EXECUTE ;")
    answer = runtime.find("ANSWER")
    assert answer is not None

    direct = runtime.new_context()
    direct.data.push(answer.xt)
    direct_result = runtime.execute("EXECUTE", context=direct)
    assert direct.data.snapshot() == (42,)
    assert direct_result.semantic_steps == 3

    compiled = runtime.new_context()
    compiled.data.push(answer.xt)
    compiled_result = runtime.execute("RUN", context=compiled)
    assert compiled.data.snapshot() == (42,)
    assert compiled_result.semantic_steps == 5


def test_comments_and_provided_are_consumed_by_the_live_source_cursor() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(
        b"\\ ignored header words\n"
        b"PROVIDED sample-module\n"
        b": KEPT ( nested ( parser ) comment ) 7 ; \\ ignored tail\n"
        b"KEPT\n",
        context=context,
    )

    assert runtime.provided_modules == frozenset({b"sample-module"})
    assert context.data.snapshot() == (7,)


def test_any_token_starting_with_backslash_ends_the_bios_input_line() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(
        b"1 \\comment this is never looked up\n"
        b"2 \\\\ nor is this\n"
        b"3\n",
        context=context,
    )

    assert context.data.snapshot() == (1, 2, 3)


def test_number_parser_uses_bios_minus_hex_prefix_and_current_base() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(
        b"HEX ff -A DECIMAL 10 -2 -0xA",
        context=context,
    )
    runtime.set_numeric_base(36)
    runtime.evaluate(b"z", context=context)

    assert runtime.numeric_base == 36
    assert context.data.snapshot() == (
        255,
        MASK64 - 9,
        10,
        MASK64 - 1,
        MASK64 - 9,
        35,
    )


@pytest.mark.parametrize(
    "token",
    (b"+1", b"1_0", b"0x", b"0x+1", b"0x0x1", b"12Z", b"1\t2"),
)
def test_number_parser_rejects_forms_the_bios_rejects(token: bytes) -> None:
    runtime = MegaForthRuntime()

    with pytest.raises(SourceError, match="unknown word"):
        runtime.evaluate(token)


def test_number_parser_wraps_and_honors_unusual_unsigned_base_cells() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(
        b"18446744073709551616 0x10000000000000000",
        context=context,
    )
    runtime.set_numeric_base(1)
    runtime.evaluate(b"000", context=context)
    runtime.set_numeric_base(0)
    runtime.evaluate(b"0xF", context=context)

    assert context.data.snapshot() == (0, 0, 0, 15)
    with pytest.raises(SourceError, match="unknown word"):
        runtime.evaluate(b"0", context=context)


def test_dictionary_lookup_precedes_numbers_and_literals_capture_compile_base() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(
        b": 10 77 ; HEX : CAPTURED A ; DECIMAL 10 CAPTURED",
        context=context,
    )

    assert runtime.numeric_base == 10
    assert context.data.snapshot() == (77, 10)


def test_compiled_hex_changes_base_only_when_the_definition_executes() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(b": SWITCH HEX ; SWITCH ff", context=context)

    assert runtime.numeric_base == 16
    assert context.data.snapshot() == (255,)


def test_counted_loops_use_one_ordered_return_stack_for_i_j_and_user_cells() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(
        b": COUNT 3 0 DO I LOOP ; "
        b": NESTED 2 0 DO 4 2 DO J I LOOP LOOP ; "
        b": RS 3 0 DO I >R R@ R> LOOP ;"
    )

    count = runtime.new_context()
    runtime.execute("COUNT", context=count)
    assert count.data.snapshot() == (0, 1, 2)
    assert count.returns.snapshot() == ()

    nested = runtime.new_context()
    runtime.execute("NESTED", context=nested)
    assert nested.data.snapshot() == (0, 2, 0, 3, 1, 2, 1, 3)
    assert nested.returns.snapshot() == ()

    user_cells = runtime.new_context()
    runtime.execute("RS", context=user_cells)
    assert user_cells.data.snapshot() == (0, 0, 1, 1, 2, 2)
    assert user_cells.returns.snapshot() == ()


def test_unloop_removes_exact_frame_before_early_exit() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": EARLY 4 0 DO I UNLOOP EXIT LOOP ;")
    context = runtime.new_context()

    runtime.execute("EARLY", context=context)

    assert context.data.snapshot() == (0,)
    assert context.returns.snapshot() == ()


def test_i_does_not_search_through_a_factored_colon_continuation() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": HELPER I ; : BAD 2 0 DO HELPER LOOP ;")

    with pytest.raises(ReturnStackShapeError, match="I.*continuation"):
        runtime.execute("BAD", context=runtime.new_context())


def test_source_errors_carry_byte_cursor_diagnostics() -> None:
    runtime = MegaForthRuntime()

    with pytest.raises(SourceError) as caught:
        runtime.evaluate(b"1\nMISSING\n", source_name="broken.f")

    assert caught.value.location.source_name == "broken.f"
    assert caught.value.location.line == 2
    assert caught.value.location.column == 0
    assert "broken.f:2:1" in str(caught.value)


def test_execution_budget_is_caller_owned_and_has_no_hidden_default_cap() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": FOREVER 0 0 DO LOOP ; : FINITE 7 ;")
    context = runtime.new_context()

    with pytest.raises(StepBudgetExceeded) as caught:
        runtime.execute(
            "FOREVER",
            context=context,
            step_budget=12,
        )

    assert caught.value.budget == 12
    assert context.returns.snapshot() == ()

    runtime.execute("FINITE", context=context)
    assert context.data.snapshot() == (7,)
    assert context.returns.snapshot() == ()


def test_evaluation_budget_is_shared_across_interpreted_tokens() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    with pytest.raises(StepBudgetExceeded):
        runtime.evaluate(
            b": TWO 1 1 ; TWO TWO",
            context=context,
            step_budget=5,
        )

    assert context.data.snapshot() == (1, 1, 1, 1)
    assert context.returns.snapshot() == ()


def test_evaluation_budget_bounds_an_interpreted_infinite_word() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    with pytest.raises(StepBudgetExceeded):
        runtime.evaluate(
            b": FOREVER 0 0 DO LOOP ; FOREVER",
            context=context,
            step_budget=10,
        )

    assert context.returns.snapshot() == ()


def test_unresolved_control_flow_is_not_published() -> None:
    runtime = MegaForthRuntime()

    with pytest.raises(SourceError, match="unresolved control flow"):
        runtime.evaluate(b": BROKEN IF 1 ;", source_name="compile.f")

    assert runtime.find("BROKEN") is None
