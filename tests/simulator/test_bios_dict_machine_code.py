"""DICT-MACHINE-CODE? in the hosted simulator.

The BIOS answers true: it compiles every definition to MP64 machine code.
This runtime keeps a definition as semantic operations, so it answers false,
and a library that builds or loads machine-code dictionary images, such as
Akashic's ``binimg``, refuses on that answer.
"""

from __future__ import annotations

from simulator.runtime import ColonDefinition, MegaForthRuntime


def test_the_hosted_dictionary_is_not_machine_code() -> None:
    runtime = MegaForthRuntime()

    runtime.execute("DICT-MACHINE-CODE?")

    assert runtime.main_context.data.snapshot() == (0,)
    assert runtime.main_context.returns.snapshot() == ()


def test_compiled_code_gets_the_same_answer_and_is_semantic_itself() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(
        b": ASKED DICT-MACHINE-CODE? ;\n: BOTH ASKED DICT-MACHINE-CODE? ;\n",
        source_name="asked",
    )

    runtime.execute("BOTH")

    assert runtime.main_context.data.snapshot() == (0, 0)
    # What the answer states: the definition is semantic operations.  Its
    # eight-byte code slot holds no instruction for a machine to run.
    word = runtime.find("ASKED")
    assert word is not None
    assert type(word.implementation) is ColonDefinition


def test_the_word_takes_nothing_from_an_empty_stack() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.execute("DICT-MACHINE-CODE?", context=context)
    runtime.execute("DICT-MACHINE-CODE?", context=context)

    assert context.data.snapshot() == (0, 0)
