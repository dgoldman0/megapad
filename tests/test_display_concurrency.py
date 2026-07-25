"""Display behavior at native execution and shared-device boundaries."""

from __future__ import annotations

import sys
from types import SimpleNamespace

import pytest

from _mp64_accel import ExternalEventKind
from devices import FramebufferDevice
from display import (
    FramebufferDisplay,
    _framebuffer_display_config,
    _framebuffer_host_present,
)
from system import MegapadSystem


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


class _AtomicFramebufferView:
    def __init__(self):
        self.presentations = 0

    def snapshot(self):
        return (0x1234, 640, 360, 2560, 3, 3, 17, False, 33333)

    def host_present(self):
        self.presentations += 1

    @property
    def enable(self):
        pytest.fail("display config must not use piecemeal properties")

    @property
    def width(self):
        pytest.fail("display config must not use piecemeal properties")

    @property
    def height(self):
        pytest.fail("display config must not use piecemeal properties")

    @property
    def mode(self):
        pytest.fail("display config must not use piecemeal properties")

    @property
    def vsync_count(self):
        pytest.fail("host presentation must use one device transition")

    @property
    def vblank(self):
        pytest.fail("host presentation must use one device transition")


class _NativeGeometry:
    def __init__(self):
        self.cols = 80
        self.rows = 24
        self._req_cols = 0
        self._req_rows = 0
        self.requested = False
        self.generation = 0
        self.host_sizes = []
        self.accepted = []
        self.denials = 0

    def host_set_size(self, cols, rows):
        self.cols = cols
        self.rows = rows
        self.host_sizes.append((cols, rows))

    def has_resize_request(self):
        return self.requested

    @property
    def req_cols(self):
        return self._req_cols

    @property
    def req_rows(self):
        return self._req_rows

    def publish_request(self, cols, rows):
        self._req_cols = cols
        self._req_rows = rows
        self.requested = True
        self.generation += 1

    def cancel_request(self):
        self.requested = False
        self.generation += 1

    def snapshot_resize_request(self):
        if not self.requested:
            return None
        return self.generation, self._req_cols, self._req_rows

    def host_accept_resize_if_pending(
        self,
        generation,
        cols,
        rows,
    ):
        if not self.requested or self.generation != generation:
            return False
        self.cols = cols
        self.rows = rows
        self.accepted.append((cols, rows))
        self.requested = False
        self.generation += 1
        return True

    def host_deny_resize_if_pending(self, generation):
        if not self.requested or self.generation != generation:
            return False
        self.denials += 1
        self.requested = False
        self.generation += 1
        return True


def _display_without_runtime_dependencies():
    display = object.__new__(FramebufferDisplay)
    display._resolve_fb_mem = lambda _base: pytest.fail(
        "busy native scanout must not resolve a raw framebuffer view"
    )
    return display


def test_display_configuration_is_atomic_and_host_present_is_observer_only():
    framebuffer = _AtomicFramebufferView()

    assert _framebuffer_display_config(framebuffer) == (3, 640, 360, 3)
    _framebuffer_host_present(framebuffer)

    assert framebuffer.presentations == 0


def test_explicit_device_host_present_remains_available_for_compatibility():
    framebuffer = FramebufferDevice()

    _framebuffer_host_present(framebuffer)
    assert (framebuffer.vsync_count, framebuffer.vblank) == (0, False)

    framebuffer.host_present()
    assert (framebuffer.vsync_count, framebuffer.vblank) == (1, True)


def test_native_framebuffer_tick_matches_segmented_reference_across_frames():
    system = MegapadSystem(
        ram_size=256,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    native = system.fb
    reference = FramebufferDevice()
    for framebuffer in (native, reference):
        framebuffer.enable = 1
        framebuffer.cycles_per_frame = 10

    native.tick(37)
    for cycles in (7, 8, 22):
        reference.tick(cycles)

    assert native.snapshot() == reference.snapshot()
    assert (native.vsync_count, native.vblank) == (3, True)

    native.tick(3)
    reference.tick(3)
    assert native.snapshot() == reference.snapshot()
    assert native.vsync_count == 4


def test_native_framebuffer_tick_is_overflow_safe_for_uint64_batch():
    system = MegapadSystem(
        ram_size=256,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    framebuffer = system.fb
    framebuffer.enable = 1
    framebuffer.cycles_per_frame = 3

    framebuffer.tick((1 << 64) - 1)

    assert framebuffer.vsync_count == 0x5555_5555
    assert framebuffer.vblank


def test_busy_native_scanout_skips_frame_without_raw_memory_fallback(
    monkeypatch,
):
    monkeypatch.setitem(sys.modules, "pygame", SimpleNamespace())
    display = _display_without_runtime_dependencies()
    framebuffer = _NativeFramebuffer(
        error=RuntimeError("CPUState framebuffer render is busy")
    )

    assert display._render_fb(framebuffer, object(), 2, 1, 1) is None


def test_display_uart_geometry_transaction_composes_with_native_system_state():
    system = MegapadSystem(
        ram_size=256,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    display = _display_without_runtime_dependencies()
    display.sys = system

    system.uart_geom.host_set_size(120, 40)
    assert (
        system.cores[1]._cs.uart_geom_cols,
        system.cores[1]._cs.uart_geom_rows,
    ) == (120, 40)

    system.cores[1]._cs.uart_geom_req_cols = 100
    system.cores[1]._cs.uart_geom_req_rows = 35
    system.cores[1]._cs.uart_geom_ctrl = 0x03
    applied = []

    def apply_resize(cols, rows):
        assert system.uart_geom.has_resize_request()
        applied.append((cols, rows))

    assert display._service_uart_geom_resize_request(
        apply_resize
    ) == (100, 35, True)
    assert applied == [(100, 35)]
    assert (system.uart_geom.cols, system.uart_geom.rows) == (100, 35)
    assert not system.uart_geom.has_resize_request()
    assert system.cores[0]._cs.uart_geom_ctrl == 0x01
    assert [
        event.kind
        for event in system._native_system.external_event_history
    ] == [
        ExternalEventKind.UART_GEOMETRY,
        ExternalEventKind.UART_GEOMETRY_ACCEPT,
    ]


@pytest.mark.parametrize(
    ("cols", "rows", "accepted"),
    (
        pytest.param(20, 5, True, id="minimum"),
        pytest.param(400, 200, True, id="maximum"),
        pytest.param(19, 30, False, id="columns-below-minimum"),
        pytest.param(401, 30, False, id="columns-above-maximum"),
        pytest.param(80, 4, False, id="rows-below-minimum"),
        pytest.param(80, 201, False, id="rows-above-maximum"),
    ),
)
def test_uart_firmware_resize_is_conditionally_acknowledged_after_host_apply(
    cols,
    rows,
    accepted,
):
    display = _display_without_runtime_dependencies()
    geometry = _NativeGeometry()
    geometry.publish_request(cols, rows)
    display.sys = SimpleNamespace(uart_geom=geometry)

    applied = []

    def apply_resize(applied_cols, applied_rows):
        assert geometry.requested
        applied.append((applied_cols, applied_rows))

    assert display._service_uart_geom_resize_request(apply_resize) == (
        cols,
        rows,
        accepted,
    )
    assert not geometry.requested
    assert applied == ([(cols, rows)] if accepted else [])
    assert geometry.accepted == ([(cols, rows)] if accepted else [])
    assert geometry.denials == (0 if accepted else 1)
    assert display._service_uart_geom_resize_request(
        lambda *_args: pytest.fail("a completed request must not repeat")
    ) is None


def test_uart_firmware_resize_remains_pending_when_host_resize_fails():
    display = _display_without_runtime_dependencies()
    geometry = _NativeGeometry()
    geometry.publish_request(120, 40)
    display.sys = SimpleNamespace(uart_geom=geometry)

    def fail_resize(cols, rows):
        assert geometry.requested
        assert (cols, rows) == (120, 40)
        raise RuntimeError("host resize failed")

    with pytest.raises(RuntimeError, match="^host resize failed$"):
        display._service_uart_geom_resize_request(fail_resize)

    assert geometry.requested
    assert geometry.accepted == []
    assert geometry.denials == 0

    applied = []
    assert display._service_uart_geom_resize_request(
        lambda cols, rows: applied.append((cols, rows))
    ) == (120, 40, True)
    assert applied == [(120, 40)]
    assert geometry.accepted == [(120, 40)]
    assert not geometry.requested


def test_uart_resize_reconciles_host_size_without_acknowledging_replacement():
    display = _display_without_runtime_dependencies()
    geometry = _NativeGeometry()
    geometry.publish_request(120, 40)
    display.sys = SimpleNamespace(uart_geom=geometry)
    host_sizes = []

    def replace_request(cols, rows):
        host_sizes.append((cols, rows))
        geometry.publish_request(401, 30)

    assert display._service_uart_geom_resize_request(replace_request) is None
    assert geometry.requested
    assert (geometry.req_cols, geometry.req_rows) == (401, 30)
    assert (geometry.cols, geometry.rows) == (120, 40)
    assert geometry.host_sizes == [(120, 40)]
    assert host_sizes == [(120, 40)]
    assert geometry.accepted == []

    assert display._service_uart_geom_resize_request(
        lambda *_args: pytest.fail("an invalid request must not resize the host")
    ) == (401, 30, False)
    assert (geometry.cols, geometry.rows) == (120, 40)
    assert geometry.accepted == []
    assert geometry.denials == 1
    assert not geometry.requested


def test_uart_resize_reconciles_host_size_after_firmware_cancellation():
    system = MegapadSystem(
        ram_size=256,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    display = _display_without_runtime_dependencies()
    display.sys = system
    core = system.cores[1]._cs
    system.uart_geom.host_set_size(80, 24)
    core.uart_geom_status = 0
    core.uart_geom_req_cols = 120
    core.uart_geom_req_rows = 40
    core.uart_geom_ctrl = 0xA3
    host_sizes = []

    def cancel_request(cols, rows):
        host_sizes.append((cols, rows))
        core.uart_geom_ctrl = 0xA1

    assert display._service_uart_geom_resize_request(cancel_request) is None
    assert host_sizes == [(120, 40)]
    assert (
        system.uart_geom.cols,
        system.uart_geom.rows,
        system.uart_geom.status,
        system.uart_geom.ctrl,
    ) == (120, 40, 0x01, 0xA1)
    assert system.uart_geom.snapshot_resize_request() is None


def test_uart_resize_with_no_request_does_not_touch_the_host():
    display = _display_without_runtime_dependencies()
    geometry = _NativeGeometry()
    display.sys = SimpleNamespace(uart_geom=geometry)

    assert display._service_uart_geom_resize_request(
        lambda *_args: pytest.fail("no host resize should be attempted")
    ) is None
    assert geometry.accepted == []
    assert geometry.denials == 0


def test_display_backend_resize_failure_is_retryable_without_partial_state():
    class HostResizeError(RuntimeError):
        pass

    display = _display_without_runtime_dependencies()
    geometry = _NativeGeometry()
    geometry.publish_request(120, 40)
    display.sys = SimpleNamespace(uart_geom=geometry)
    resize_calls = []
    layout_calls = []
    status_messages = []
    display.term = SimpleNamespace(
        resize=lambda cols, rows: resize_calls.append((cols, rows)))
    display.debug = SimpleNamespace(visible=True, width=90)
    display.status = SimpleNamespace(set_message=status_messages.append)
    menubar = SimpleNamespace(
        layout=lambda *_args: layout_calls.append(_args))

    def reject_mode(size, flags):
        assert size == (120 * 8 + 90, 40 * 16 + 30)
        assert flags == 7
        raise HostResizeError("backend rejected size")

    pygame = SimpleNamespace(
        RESIZABLE=7,
        display=SimpleNamespace(set_mode=reject_mode),
    )

    def apply_resize(cols, rows):
        display._apply_uart_geom_resize(
            pygame,
            menubar,
            object(),
            8,
            16,
            30,
            cols,
            rows,
        )

    assert display._service_uart_geom_resize_request_safely(
        apply_resize, HostResizeError
    ) is None
    assert geometry.requested
    assert geometry.accepted == []
    assert resize_calls == []
    assert layout_calls == []
    assert status_messages == ["Resize failed: backend rejected size"]


def test_display_backend_resize_applies_staged_state_in_order():
    display = _display_without_runtime_dependencies()
    events = []
    screen = object()
    ui_font = object()
    display.term = SimpleNamespace(
        resize=lambda cols, rows: events.append(
            ("term", cols, rows)))
    display.debug = SimpleNamespace(visible=True, width=90)
    menubar = SimpleNamespace(
        layout=lambda pygame, font, width: events.append(
            ("menu", pygame, font, width)))

    def set_mode(size, flags):
        events.append(("mode", size, flags))
        return screen

    pygame = SimpleNamespace(
        RESIZABLE=7,
        display=SimpleNamespace(set_mode=set_mode),
    )

    result = display._apply_uart_geom_resize(
        pygame,
        menubar,
        ui_font,
        8,
        16,
        30,
        120,
        40,
    )

    assert result == (screen, 960, 1050, 670)
    assert events == [
        ("mode", (1050, 670), 7),
        ("term", 120, 40),
        ("menu", pygame, ui_font, 1050),
    ]


def test_native_uart_resize_failure_preserves_device_state_for_retry():
    system = MegapadSystem(
        ram_size=256,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    display = _display_without_runtime_dependencies()
    display.sys = system
    core = system.cores[1]._cs
    system.uart_geom.host_set_size(90, 30)
    core.uart_geom_status = 0xA2
    core.uart_geom_req_cols = 120
    core.uart_geom_req_rows = 40
    core.uart_geom_ctrl = 0xA3
    before = (
        system.uart_geom.cols,
        system.uart_geom.rows,
        system.uart_geom.status,
        system.uart_geom.ctrl,
        system.uart_geom.req_cols,
        system.uart_geom.req_rows,
    )

    def fail_resize(_cols, _rows):
        raise RuntimeError("host resize failed")

    with pytest.raises(RuntimeError, match="^host resize failed$"):
        display._service_uart_geom_resize_request(fail_resize)

    assert (
        system.uart_geom.cols,
        system.uart_geom.rows,
        system.uart_geom.status,
        system.uart_geom.ctrl,
        system.uart_geom.req_cols,
        system.uart_geom.req_rows,
    ) == before

    assert display._service_uart_geom_resize_request(
        lambda *_args: None
    ) == (120, 40, True)
    assert (
        system.uart_geom.cols,
        system.uart_geom.rows,
        system.uart_geom.status,
        system.uart_geom.ctrl,
    ) == (120, 40, 0xA3, 0xA1)


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
