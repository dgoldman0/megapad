"""Framebuffer display behavior at native execution boundaries."""

from __future__ import annotations

import sys
from types import SimpleNamespace

import pytest

from display import FramebufferDisplay


class _NativeFramebuffer:
    def __init__(self, *, error: RuntimeError | None = None, result=None):
        self._cs = SimpleNamespace(render_fb_rgb=self._render)
        self._error = error
        self._result = result

    def _render(self):
        if self._error is not None:
            raise self._error
        return self._result

    @property
    def fb_base(self):
        pytest.fail("busy native scanout must not use the raw-memory fallback")


def _display_without_runtime_dependencies():
    display = object.__new__(FramebufferDisplay)
    display._resolve_fb_mem = lambda _base: pytest.fail(
        "busy native scanout must not resolve a raw framebuffer view"
    )
    return display


def test_busy_native_scanout_skips_frame_without_raw_memory_fallback(
    monkeypatch,
):
    monkeypatch.setitem(sys.modules, "pygame", SimpleNamespace())
    display = _display_without_runtime_dependencies()
    framebuffer = _NativeFramebuffer(
        error=RuntimeError("CPUState framebuffer render is busy")
    )

    assert display._render_fb(framebuffer, object(), 2, 1, 1) is None


def test_native_scanout_does_not_hide_unrelated_runtime_errors(monkeypatch):
    monkeypatch.setitem(sys.modules, "pygame", SimpleNamespace())
    display = _display_without_runtime_dependencies()
    framebuffer = _NativeFramebuffer(error=RuntimeError("conversion failed"))

    with pytest.raises(RuntimeError, match="^conversion failed$"):
        display._render_fb(framebuffer, object(), 2, 1, 1)


@pytest.mark.parametrize(
    "result",
    (
        pytest.param(None, id="native-none"),
        pytest.param([[[0, 0, 0]]], id="stale-native-shape"),
    ),
)
def test_native_scanout_never_falls_through_to_raw_memory(
    monkeypatch, result
):
    monkeypatch.setitem(
        sys.modules,
        "pygame",
        SimpleNamespace(
            surfarray=SimpleNamespace(
                blit_array=lambda *_args: pytest.fail(
                    "an incompatible native result must skip the frame"
                )
            )
        ),
    )
    display = _display_without_runtime_dependencies()
    framebuffer = _NativeFramebuffer(result=result)

    assert display._render_fb(framebuffer, object(), 2, 1, 1) is None
