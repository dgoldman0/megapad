"""Focused compiler and explicit-dispatch tests for the hosted runtime."""

from __future__ import annotations

import pytest

from shared.cells import MASK64, TRUE
from simulator.errors import ExecutionError, SourceError, StepBudgetExceeded
from simulator.ir import Loop, QuestionDo
from simulator.memory import SparseAddressSpace
from simulator.runtime import ColonDefinition, ConstantDefinition, MegaForthRuntime
from simulator.stacks import ReturnStackShapeError


def test_runtime_owns_a_default_address_space_or_uses_the_injected_one() -> None:
    first = MegaForthRuntime()
    second = MegaForthRuntime()
    injected = SparseAddressSpace(bank0_size=0x80)

    assert isinstance(first.memory, SparseAddressSpace)
    assert first.memory is not second.memory
    assert MegaForthRuntime(memory=injected).memory is injected

    with pytest.raises(TypeError, match="SparseAddressSpace"):
        MegaForthRuntime(memory=object())  # type: ignore[arg-type]


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
