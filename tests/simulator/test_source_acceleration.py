"""Focused differential coverage for KDOS source-walking acceleration."""

from __future__ import annotations

from shared.cells import TRUE, u64
from simulator.runtime import (
    ColonDefinition,
    ConstantDefinition,
    CreatedDefinition,
    MegaForthRuntime,
    ValueDefinition,
)
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
    b"EVALUATE-CHECKED",
    b"SOURCE-EVALUATE-CHECKED",
    b"_LD-STATUS-THROW",
    b"_CRC-BUF-CHECKED",
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


def _run_crc_feed(runtime: MegaForthRuntime, name: str, payload: bytes):
    source = runtime.define_created(name, initial_body=payload)
    identity = runtime.guest_identity(runtime.main_context)
    assert runtime.crc.select_mode(identity, 4) == 0
    assert runtime.crc.seed(identity, 0xFFFF_FFFF) == 0
    runtime.main_context.data.push(source.body_address)
    runtime.main_context.data.push(len(payload))
    result = runtime.execute("_CRC-BUF-CHECKED")
    stack = runtime.main_context.data.snapshot()
    runtime.main_context.data.clear()
    accumulator = runtime.crc.accumulator
    runtime.crc.final(identity)
    return result.semantic_steps, stack, accumulator


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


def test_checked_line_acceleration_preserves_safe_interpreted_definitions() -> None:
    source = (
        b"11 22 + CONSTANT LOADED-CONSTANT\n"
        b"VARIABLE LOADED-VARIABLE\n"
        b"99 VALUE LOADED-VALUE\n"
        b"LOADED-CONSTANT LOADED-VALUE +\n"
    )
    ordinary = _load_module_system()
    accelerated = _load_module_system()
    install_kdos_source_accelerators(accelerated)

    slow = _run_checked(ordinary, "INTERPRETED-SOURCE", source)
    fast = _run_checked(accelerated, "INTERPRETED-SOURCE", source)

    assert fast[1:] == slow[1:]
    assert fast[1] == (132, 0)
    assert fast[0] < slow[0]
    for runtime in (ordinary, accelerated):
        constant = runtime.find("LOADED-CONSTANT")
        variable = runtime.find("LOADED-VARIABLE")
        value = runtime.find("LOADED-VALUE")
        assert constant is not None
        assert variable is not None
        assert value is not None
        assert isinstance(constant.implementation, ConstantDefinition)
        assert isinstance(variable.implementation, CreatedDefinition)
        assert isinstance(value.implementation, ValueDefinition)
        assert _execute(runtime, "LOADED-CONSTANT") == (33,)
        assert _execute(runtime, "LOADED-VARIABLE") == (
            variable.body_address,
        )
        assert runtime.memory.read64(variable.body_address) == 0
        assert _execute(runtime, "LOADED-VALUE") == (99,)


def test_source_accelerators_retain_definition_bound_state_after_shadows() -> None:
    ordinary = _load_module_system()
    accelerated = _load_module_system()
    sentinel = 0xA55A_1122_3344_7788
    initial_body = sentinel.to_bytes(8, "little")
    for runtime in (ordinary, accelerated):
        runtime.define_created("EVAL-STATUS", initial_body=initial_body)
        runtime.define_created("EVAL-LINE", initial_body=initial_body)
        runtime.define_created("_SEC-CUR", initial_body=initial_body)
    install_kdos_source_accelerators(accelerated)

    slow = _run_checked(ordinary, "SHADOWED-STATE-SOURCE", b"1 2 +\n")
    fast = _run_checked(accelerated, "SHADOWED-STATE-SOURCE", b"1 2 +\n")

    assert fast[1:] == slow[1:]
    assert fast[1] == (3, 0)
    assert fast[2][0:2] == (sentinel, sentinel)
    assert fast[2][5] == sentinel


def test_loader_crc_acceleration_preserves_cells_tail_and_owner_failure() -> None:
    payload = bytes((index * 29 + 7) & 0xFF for index in range(4099))
    ordinary = _load_module_system()
    accelerated = _load_module_system()
    install_kdos_source_accelerators(accelerated)

    slow = _run_crc_feed(ordinary, "SLOW-CRC-SOURCE", payload)
    fast = _run_crc_feed(accelerated, "FAST-CRC-SOURCE", payload)

    assert fast[1:] == slow[1:]
    assert fast[1] == (0,)
    assert fast[0] * 10 < slow[0]
    assert _execute(ordinary, "_CRC-BUF-CHECKED", 0, 1) == (2,)
    assert _execute(accelerated, "_CRC-BUF-CHECKED", 0, 1) == (2,)


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
    sentinel = 0x55AA_8877_6655_4433
    for runtime in runtimes:
        runtime.define_created(
            "LD-CUR",
            initial_body=sentinel.to_bytes(8, "little"),
        )
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


def test_compile_line_fast_path_retains_immediate_throw_handling() -> None:
    source = (
        b": NEVER-PUBLISHED\n"
        b"DUP COMPILE-TIME-THROW\n"
        b";\n"
    )
    ordinary = _load_module_system()
    accelerated = _load_module_system()
    for runtime in (ordinary, accelerated):
        def compile_time_throw(context, *, owner=runtime) -> None:
            context.data.push(u64(-77))
            owner.execute("THROW", context=context)

        runtime.define_primitive(
            "COMPILE-TIME-THROW",
            compile_time_throw,
            immediate=True,
        )
    install_kdos_source_accelerators(accelerated)

    slow = _run_checked(ordinary, "SLOW-IMMEDIATE-THROW", source)
    fast = _run_checked(accelerated, "FAST-IMMEDIATE-THROW", source)

    assert fast[1:] == slow[1:]
    assert fast[1] == (5,)
    assert fast[2][4] == u64(-77)
    assert ordinary.find("NEVER-PUBLISHED") is None
    assert accelerated.find("NEVER-PUBLISHED") is None


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
