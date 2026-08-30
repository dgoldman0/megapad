"""Focused contract tests for the deliberately temporary source loader."""

from __future__ import annotations

import pytest

from simulator.bootstrap_loader import (
    BootstrapLoadError,
    BootstrapModule,
    BootstrapSourceLoader,
)
from simulator.errors import StepBudgetExceeded
from simulator.runtime import MegaForthRuntime


def _module(
    request_name: bytes,
    provided_id: bytes,
    source: bytes,
) -> BootstrapModule:
    return BootstrapModule(
        request_name=request_name,
        provided_id=provided_id,
        source_name=f"fixtures/{request_name.decode('ascii')}",
        source=source,
    )


@pytest.mark.parametrize(
    ("second", "message"),
    (
        (_module(b"same.f", b"second-id", b""), "request name"),
        (_module(b"other.f", b"same-id", b""), "provided ID"),
    ),
)
def test_module_table_rejects_ambiguous_duplicate_identities(
    second: BootstrapModule,
    message: str,
) -> None:
    runtime = MegaForthRuntime()
    first = _module(b"same.f", b"same-id", b"")

    with pytest.raises(ValueError, match=message):
        BootstrapSourceLoader(runtime, (first, second))


def test_nested_require_uses_distinct_provided_ids_and_skips_completed_loads() -> None:
    runtime = MegaForthRuntime()
    loader = BootstrapSourceLoader(
        runtime,
        (
            _module(
                b"dependency.f",
                b"dependency-id",
                b"PROVIDED dependency-id\n"
                b": DEPENDENCY-VALUE 23 ;\n"
                b"DEPENDENCY-VALUE\n",
            ),
            _module(
                b"root.f",
                b"root-id",
                b"PROVIDED root-id\n"
                b"REQUIRE dependency.f\n"
                b"REQUIRE dependency.f\n"
                b": ROOT-VALUE 41 ;\n",
            ),
        ),
    )
    require = loader.install()

    assert require.immediate is False
    result = loader.load(b"root.f")

    assert result is not None
    assert result.source_name == "fixtures/root.f"
    assert [word.name for word in result.definitions] == [b"ROOT-VALUE"]
    assert runtime.main_context.data.snapshot() == (23,)
    assert runtime.provided_modules == frozenset(
        {b"dependency-id", b"root-id"}
    )
    assert loader.load(b"root.f") is None


def test_compiled_require_keeps_its_bootstrap_xt_after_name_shadowing() -> None:
    runtime = MegaForthRuntime()
    loader = BootstrapSourceLoader(
        runtime,
        (_module(b"target.f", b"target-id", b"PROVIDED target-id"),),
    )
    bootstrap_word = loader.install()
    runtime.evaluate(b": OLD-REQUIRE REQUIRE ;")

    replacement_requests: list[bytes] = []

    def replacement(_context) -> None:
        replacement_requests.append(
            runtime.parse_required_input_word(b"replacement REQUIRE")
        )

    replacement_word = runtime.define_primitive(b"REQUIRE", replacement)
    assert replacement_word.xt != bootstrap_word.xt

    runtime.evaluate(b"OLD-REQUIRE target.f")
    assert b"target-id" in runtime.provided_modules
    assert replacement_requests == []

    runtime.evaluate(b"REQUIRE bypass.f")
    assert replacement_requests == [b"bypass.f"]


def test_missing_nested_source_rolls_back_outer_publication_and_cleans_state() -> None:
    runtime = MegaForthRuntime()
    loader = BootstrapSourceLoader(
        runtime,
        (
            _module(
                b"outer.f",
                b"outer-id",
                b"PROVIDED outer-id REQUIRE absent.f",
            ),
        ),
    )
    loader.install()

    for _ in range(2):
        with pytest.raises(BootstrapLoadError, match="no registered source"):
            loader.load(b"outer.f")
        assert b"outer-id" not in runtime.provided_modules

    with pytest.raises(BootstrapLoadError, match="ghost.f"):
        loader.load(b"ghost.f")


def test_dependency_cycles_roll_back_each_provisional_id_and_are_retryable() -> None:
    runtime = MegaForthRuntime()
    loader = BootstrapSourceLoader(
        runtime,
        (
            _module(
                b"a.f",
                b"a-id",
                b"PROVIDED a-id REQUIRE b.f",
            ),
            _module(
                b"b.f",
                b"b-id",
                b"PROVIDED b-id REQUIRE a.f",
            ),
        ),
    )
    loader.install()

    for _ in range(2):
        with pytest.raises(
            BootstrapLoadError,
            match=r"dependency cycle: a\.f -> b\.f -> a\.f",
        ):
            loader.load(b"a.f")
        assert runtime.provided_modules.isdisjoint({b"a-id", b"b-id"})


@pytest.mark.parametrize(
    "source",
    (
        b": SURVIVES 1 ;",
        b"PROVIDED typo-id : SURVIVES 1 ;",
        b"PROVIDED required-id PROVIDED extra-id : SURVIVES 1 ;",
    ),
)
def test_source_must_publish_only_declared_id_without_dictionary_rollback_claim(
    source: bytes,
) -> None:
    runtime = MegaForthRuntime()
    loader = BootstrapSourceLoader(
        runtime,
        (_module(b"unprovided.f", b"required-id", source),),
    )

    for _ in range(2):
        previous = runtime.dictionary.here
        with pytest.raises(BootstrapLoadError, match="required PROVIDED ID"):
            loader.load(b"unprovided.f")
        assert runtime.dictionary.here > previous
        assert runtime.provided_modules.isdisjoint(
            {b"required-id", b"typo-id", b"extra-id"}
        )


def test_nested_loads_share_budget_and_completed_dependency_survives_failure() -> None:
    runtime = MegaForthRuntime()
    loader = BootstrapSourceLoader(
        runtime,
        (
            _module(
                b"dependency.f",
                b"dependency-id",
                b"PROVIDED dependency-id 1 DROP",
            ),
            _module(
                b"root.f",
                b"root-id",
                b"PROVIDED root-id REQUIRE dependency.f 1 DROP",
            ),
        ),
    )
    loader.install()

    with pytest.raises(StepBudgetExceeded):
        loader.load(b"root.f", step_budget=2)

    assert b"dependency-id" in runtime.provided_modules
    assert b"root-id" not in runtime.provided_modules

    retry = loader.load(b"root.f", step_budget=2)
    assert retry is not None
    assert retry.semantic_steps == 2
    assert b"root-id" in runtime.provided_modules


def test_failed_outer_owns_extra_ids_but_not_completed_dependency_ids() -> None:
    runtime = MegaForthRuntime()
    loader = BootstrapSourceLoader(
        runtime,
        (
            _module(
                b"dependency.f",
                b"dependency-id",
                b"PROVIDED dependency-id",
            ),
            _module(
                b"root.f",
                b"root-id",
                b"PROVIDED root-id REQUIRE dependency.f PROVIDED extra-id",
            ),
        ),
    )
    loader.install()

    with pytest.raises(BootstrapLoadError, match="required PROVIDED ID"):
        loader.load(b"root.f")

    assert runtime.provided_modules == frozenset({b"dependency-id"})
