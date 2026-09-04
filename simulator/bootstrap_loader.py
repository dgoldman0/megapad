"""Narrow host-backed source loader used only before KDOS is available.

This adapter gives early semantic source qualification an ordinary,
shadowable ``REQUIRE`` word without pretending to implement the KDOS module
system.  Its module table is supplied explicitly by the caller: there is no
filesystem lookup, path resolution, prescan, or dictionary transaction here.
KDOS is expected to replace this word and own those policies later in boot.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable

from simulator.dictionary import Word
from simulator.errors import ExecutionError
from simulator.runtime import (
    EvaluationResult,
    ExecutionContext,
    MegaForthRuntime,
)


def _module_name(value: bytes, *, field: str) -> bytes:
    if not isinstance(value, bytes):
        raise TypeError(f"{field} must be bytes")
    if not value:
        raise ValueError(f"{field} must not be empty")
    return value


@dataclass(frozen=True, slots=True)
class BootstrapModule:
    """One exact request-to-source record supplied by the host harness.

    ``request_name`` is the token consumed by ``REQUIRE``.  ``provided_id``
    is deliberately separate because real Akashic sources commonly request a
    filename such as ``uint-range.f`` while publishing an identifier such as
    ``akashic-uint-range``.  Outside completed dependencies, the source must
    publish exactly this one ID.
    """

    request_name: bytes
    provided_id: bytes
    source_name: str
    source: bytes

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "request_name",
            _module_name(self.request_name, field="request name"),
        )
        object.__setattr__(
            self,
            "provided_id",
            _module_name(self.provided_id, field="provided ID"),
        )
        if not isinstance(self.source_name, str):
            raise TypeError("source name must be str")
        if not self.source_name:
            raise ValueError("source name must not be empty")
        if not isinstance(self.source, bytes):
            raise TypeError("source must be bytes")


class BootstrapLoadError(ExecutionError):
    """An explicitly registered bootstrap module could not be loaded."""


@dataclass(slots=True)
class _LoadFrame:
    request_name: bytes
    completed_dependency_ids: set[bytes] = field(default_factory=set)


class BootstrapSourceLoader:
    """Install and service the temporary pre-KDOS ``REQUIRE`` surface."""

    def __init__(
        self,
        runtime: MegaForthRuntime,
        modules: Iterable[BootstrapModule],
    ) -> None:
        if not isinstance(runtime, MegaForthRuntime):
            raise TypeError("runtime must be a MegaForthRuntime")

        records: dict[bytes, BootstrapModule] = {}
        provided_ids: set[bytes] = set()
        try:
            iterator = iter(modules)
        except TypeError:
            raise TypeError("modules must be an iterable of BootstrapModule") from None
        for module in iterator:
            if not isinstance(module, BootstrapModule):
                raise TypeError("modules must contain only BootstrapModule records")
            if module.request_name in records:
                raise ValueError(
                    f"duplicate bootstrap request name {module.request_name!r}"
                )
            if module.provided_id in provided_ids:
                raise ValueError(
                    f"duplicate bootstrap provided ID {module.provided_id!r}"
                )
            records[module.request_name] = module
            provided_ids.add(module.provided_id)

        self.runtime = runtime
        self._modules = records
        self._loading: list[_LoadFrame] = []

    def install(self) -> Word:
        """Publish one ordinary ``REQUIRE`` definition.

        Every call publishes a new dictionary binding.  That makes the word
        naturally shadowable while previously compiled calls retain the
        stable execution token they captured.
        """

        def require(context: ExecutionContext) -> None:
            request_name = self.runtime.parse_required_input_word(b"REQUIRE")
            self.load(request_name, context=context)

        return self.runtime.define_primitive(b"REQUIRE", require)

    def load(
        self,
        request_name: bytes,
        *,
        context: ExecutionContext | None = None,
        step_budget: int | None = None,
    ) -> EvaluationResult | None:
        """Load one exact record, or return ``None`` when already provided.

        Nested calls reached through the installed primitive intentionally
        omit a new budget, so :class:`MegaForthRuntime` reuses the active
        semantic step meter.  On failure, registry IDs introduced by this
        frame are revoked while IDs introduced by completed dependencies are
        retained.  Definitions and stack effects are intentionally not rolled
        back here.
        """

        request = _module_name(request_name, field="request name")
        loading_names = [frame.request_name for frame in self._loading]
        if request in loading_names:
            start = loading_names.index(request)
            cycle = loading_names[start:] + [request]
            chain = b" -> ".join(cycle).decode("ascii", errors="backslashreplace")
            raise BootstrapLoadError(f"bootstrap REQUIRE dependency cycle: {chain}")

        try:
            module = self._modules[request]
        except KeyError:
            raise BootstrapLoadError(
                f"bootstrap REQUIRE has no registered source for {request!r}"
            ) from None

        if module.provided_id in self.runtime.provided_modules:
            return None

        before = self.runtime.provided_modules
        frame = _LoadFrame(request)
        self._loading.append(frame)
        try:
            try:
                result = self.runtime.evaluate(
                    module.source,
                    source_name=module.source_name,
                    context=context,
                    step_budget=step_budget,
                )
                introduced = set(self.runtime.provided_modules - before)
                owned_ids = introduced - frame.completed_dependency_ids
                if owned_ids != {module.provided_id}:
                    raise BootstrapLoadError(
                        f"bootstrap source {module.source_name!r} must publish "
                        f"exactly its required PROVIDED ID {module.provided_id!r}"
                    )
                if len(self._loading) > 1:
                    self._loading[-2].completed_dependency_ids.update(introduced)
                return result
            except BaseException:
                introduced = self.runtime.provided_modules - before
                owned_ids = introduced - frame.completed_dependency_ids
                for module_id in owned_ids:
                    self.runtime.revoke_provided_module(module_id)
                raise
        finally:
            active = self._loading.pop()
            if active is not frame:
                raise AssertionError("bootstrap loading stack is corrupted")


__all__ = [
    "BootstrapLoadError",
    "BootstrapModule",
    "BootstrapSourceLoader",
]
