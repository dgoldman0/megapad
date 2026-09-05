"""Focused differential coverage for KDOS source-walking acceleration."""

from __future__ import annotations

from shared.cells import TRUE, u64
from simulator.runtime import ColonDefinition, MegaForthRuntime
from simulator.source_acceleration import install_kdos_source_accelerators
from tests.simulator.test_kdos_module_system import (
    _execute,
    _load_module_system,
    _module_image,
    _mount,
    _stack_eval,
    _variable,
)


_ACCELERATED_WORDS = (
    b"SOURCE-EVALUATE-CHECKED",
    b"_LD-WALK",
    b"_PS-LINE-LEN",
)


def _run_checked(runtime: MegaForthRuntime, name: str, source: bytes):
    source_word = runtime.define_created(name, initial_body=source)
    runtime.main_context.data.push(source_word.body_address)
    runtime.main_context.data.push(len(source))
    result = runtime.execute("SOURCE-EVALUATE-CHECKED")
    stack = runtime.main_context.data.snapshot()
    runtime.main_context.data.clear()
    state = tuple(
        _variable(runtime, variable)
        for variable in (
            "EVAL-STATUS",
            "EVAL-LINE",
            "EVAL-COLUMN",
            "EVAL-DEPTH",
            "EVAL-THROW",
            "_SEC-CUR",
            "_SEC-REM",
            "_SEC-RAW-LEN",
            "_SEC-EVAL-LEN",
            "_SEC-LINE",
        )
    )
    token_address, token_length = _execute(runtime, "EVAL-TOKEN")
    token = runtime.memory.read_bytes(token_address, token_length)
    return result.semantic_steps, stack, state, token, runtime.drain_uart_output()


def _loader_state(runtime: MegaForthRuntime):
    return (
        _execute(runtime, "_MOD-COUNT"),
        _execute(runtime, "HEAP-FREE-BYTES"),
        tuple(
            _variable(runtime, name)
            for name in (
                "_LD-SP",
                "_REQ-SP",
                "EVAL-DEPTH",
                "CWD",
                "LD-BUF",
                "LD-SZ",
                "LD-CUR",
                "LD-LEN",
                "LD-LINE",
            )
        ),
        (runtime.dictionary.here, runtime.dictionary.latest),
        runtime.storage.image_bytes,
        runtime.main_context.data.snapshot(),
        runtime.main_context.returns.snapshot(),
    )


def test_exact_kdos_walkers_install_without_replacing_source_words() -> None:
    runtime = _load_module_system()
    words = tuple(runtime.find(name) for name in _ACCELERATED_WORDS)
    implementations = tuple(word.implementation for word in words if word)

    report = install_kdos_source_accelerators(runtime)

    assert report.installed == _ACCELERATED_WORDS
    assert report.skipped == ()
    assert all(word is runtime.find(name) for word, name in zip(words, _ACCELERATED_WORDS))
    assert all(
        isinstance(word.implementation, ColonDefinition)
        for word in words
        if word is not None
    )
    assert tuple(word.implementation for word in words if word) == implementations


def test_checked_source_acceleration_matches_success_and_failure_state() -> None:
    ordinary = _load_module_system()
    accelerated = _load_module_system()
    install_kdos_source_accelerators(accelerated)

    success = b"\\ representative source-loader scan padding\n" * 128 + (
        b"\r\n"
        b": WALKED\r\n"
        b"DUP 0= IF\r\n"
        b"DROP 41 ELSE\r\n"
        b"1+ THEN ;\r\n"
        b"0 WALKED\r\n"
        b"8 WALKED"
    )
    slow_success = _run_checked(ordinary, "SLOW-SUCCESS", success)
    fast_success = _run_checked(accelerated, "SLOW-SUCCESS", success)

    assert fast_success[1:] == slow_success[1:]
    assert fast_success[1] == (41, 9, 0)
    assert fast_success[0] * 10 < slow_success[0]
    assert tuple(word.name for word in accelerated.dictionary.words) == tuple(
        word.name for word in ordinary.dictionary.words
    )

    failure = b"11 22 +\n5 missing-token 99\n77\n"
    slow_failure = _run_checked(ordinary, "SLOW-FAILURE", failure)
    fast_failure = _run_checked(accelerated, "SLOW-FAILURE", failure)

    assert fast_failure[1:] == slow_failure[1:]
    assert fast_failure[1] == (33, 5, 1)
    assert fast_failure[3] == b"missing-token"
    assert fast_failure[0] < slow_failure[0]


def test_accelerated_require_retains_fs_duplicate_and_rollback_semantics() -> None:
    parent_source = (
        b"PROVIDED parent.failed\n"
        b"REQUIRE child.f\n"
        b": PARENT-LEFT-BEHIND 91 ;\n"
        b"-77 THROW\n"
    )
    child_source = (
        b"PROVIDED child.committed\n"
        b": CHILD-COMMITTED-WORD 73 ;\n"
    )
    unfinished_source = (
        b"PROVIDED unfinished.failed\n"
        b": UNFINISHED-LEFT-BEHIND 5\n"
    )
    image = _module_image(
        (
            (b"parent.f", parent_source),
            (b"child.f", child_source),
            (b"unfinished.f", unfinished_source),
        )
    )
    runtimes = (_load_module_system(), _load_module_system())
    install_kdos_source_accelerators(runtimes[1])

    observations = []
    for runtime in runtimes:
        runtime.storage.attach(image)
        _mount(runtime)
        before = _loader_state(runtime)

        failure = _stack_eval(runtime, b"' REQUIRE CATCH parent.f")
        after_failure = _loader_state(runtime)
        assert failure == (u64(-77),)
        assert _stack_eval(runtime, b"MODULE? parent.failed") == (0,)
        assert _stack_eval(runtime, b"MODULE? child.committed") == (0,)
        assert runtime.find("CHILD-COMMITTED-WORD") is None
        assert runtime.find("PARENT-LEFT-BEHIND") is None
        assert after_failure == before

        unfinished = _stack_eval(runtime, b"' REQUIRE CATCH unfinished.f")
        after_unfinished = _loader_state(runtime)
        assert unfinished == (4,)
        assert _stack_eval(runtime, b"MODULE? unfinished.failed") == (0,)
        assert runtime.find("UNFINISHED-LEFT-BEHIND") is None
        assert after_unfinished == before

        runtime.evaluate(b"REQUIRE child.f", source_name="accelerated-child.f")
        committed = _loader_state(runtime)
        here_after_first = runtime.dictionary.here
        heap_after_first = _execute(runtime, "HEAP-FREE-BYTES")
        runtime.evaluate(b"REQUIRE child.f", source_name="duplicate-child.f")
        duplicate = _loader_state(runtime)

        assert _stack_eval(runtime, b"MODULE? child.committed") == (TRUE,)
        assert _execute(runtime, "CHILD-COMMITTED-WORD") == (73,)
        assert runtime.dictionary.here == here_after_first
        assert _execute(runtime, "HEAP-FREE-BYTES") == heap_after_first
        assert duplicate == committed
        observations.append(
            (
                failure,
                after_failure,
                unfinished,
                after_unfinished,
                committed,
                duplicate,
            )
        )

    assert observations[1] == observations[0]


def test_changed_word_is_skipped_and_rolled_back_xt_cannot_inherit_overlay() -> None:
    runtime = _load_module_system()
    runtime.evaluate(b": _PS-LINE-LEN 2DROP 77 ;")

    report = install_kdos_source_accelerators(runtime)

    assert b"_PS-LINE-LEN" not in report.installed
    assert b"_PS-LINE-LEN" in report.skipped
    assert _execute(runtime, "_PS-LINE-LEN", 0, 1) == (77,)

    checkpoint = (runtime.dictionary.here, runtime.dictionary.latest)
    runtime.evaluate(b": REUSED-XT 1 ;")
    old = runtime.find(b"REUSED-XT")
    assert old is not None
    old_xt = old.xt
    runtime.install_colon_accelerator(
        old,
        applicable=lambda _context: True,
        callback=lambda context: context.data.push(99),
    )
    assert _execute(runtime, "REUSED-XT") == (99,)

    assert _execute(runtime, "DICT-ROLLBACK", *checkpoint) == ()
    runtime.evaluate(b": REUSED-XT 2 ;")
    replacement = runtime.find(b"REUSED-XT")
    assert replacement is not None
    assert replacement.xt == old_xt
    assert _execute(runtime, "REUSED-XT") == (2,)
