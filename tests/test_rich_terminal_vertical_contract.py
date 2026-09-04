"""Fast source/document guards for rich-terminal architecture boundaries."""

from __future__ import annotations

import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
RICH_DOCS = ROOT / "docs" / "rich-terminal"


def _read(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def test_semantic_guest_and_renderer_path_stays_architecturally_aligned() -> None:
    """Keep the implemented semantic path and glyph fallback aligned."""

    module = _read(RICH_DOCS / "RICH-TERMINAL-MODULE.md")
    retained = _read(RICH_DOCS / "APT-1-RETAINED-1.md")
    wire = _read(RICH_DOCS / "APT-1-WIRE.md")
    host_port = _read(RICH_DOCS / "MEGAPAD-TERMINAL-HOST-PORT.md")
    guest = _read(ROOT / "rich-terminal.f")
    projection = _read(ROOT / "rich_terminal" / "retained_view.py")
    scene = _read(ROOT / "rich_terminal" / "retained_scene.py")
    compositor = _read(ROOT / "rich_terminal" / "pygame_view.py")
    final_raster = _read(ROOT / "rich_terminal" / "final_raster.py")
    server = _read(ROOT / "rich_terminal" / "server.py")
    session = _read(ROOT / "session.py")
    viewer = _read(ROOT / "session_viewer.py")

    # The semantic family and its renderer now exist, while glyph transport
    # remains only the complete visual fallback. Keep those implementation
    # boundaries moving together as later control families and renderers land.
    assert "typed MENU_BAR/MENU/MENU_ITEM/MENU_SEPARATOR writers" in module
    assert "GLYPH_RUN remains the complete visual" in module
    assert "drawing a menu only as" in module
    assert "This profile defines the required first in-place semantic-control" in retained
    assert "`4000`–`4FFF` | Semantic controls reserved for additive profiles." in wire
    assert re.search(
        r"complete GLYPH_RUN-only screen\s+cannot substitute",
        host_port,
    )
    assert "PT-GLYPH-RUN-DEFINE" in guest
    assert "class MenuBarDraw" in projection
    assert "def _project_menu_bar" in projection
    assert "is unsupported by draw-plane rendering" in projection
    assert "renderer-owned code-native" in module
    assert "PT-CONTROL-DEFINE" in guest
    assert "PT-CONTROL-REPLACE" in guest
    assert "PT-CONTROL-DROP" in guest
    assert "PT-CONTROL-EVENT-OWNER@" in guest
    assert "def require_interactable_control" in scene
    assert "class ControlHitTarget" in compositor
    assert "class CompositeDrawResult" in compositor
    assert "def composite_draw_plane_result" in compositor
    assert "def _paint_menu_bar" in compositor
    assert "_ACCENT = (78, 139, 246, 255)" in compositor
    assert "def send_control_event" in server
    assert "MessageType.CONTROL_EVENT" in server
    assert 'pygame.font.SysFont("sans"' in viewer
    assert "def stage_frame_hit_map" in viewer
    assert "def capture_final_terminal_raster" in viewer
    assert "def finish_presentation" in viewer
    assert "class _SemanticPointerInteractor" in viewer
    assert "class FinalRasterDisplayState" in final_raster
    assert "def derive_raster_damage" in final_raster
    assert "def _acknowledged_output_scope" in session
    assert "def send_control_event" in session

    object_table = retained.split("Object type values are:", 1)[1].split(
        "### 11.1 GROUP", 1
    )[0]
    object_names = re.findall(r"^\|\s*\d+\s*\|\s*`([A-Z_]+)`", object_table, re.MULTILINE)
    assert object_names == [
        "GROUP",
        "POLYLINE",
        "IMAGE",
        "GLYPH_RUN",
        "READOUT",
        "METER",
        "STATUS",
        "PLOT",
        "WAVEFORM",
    ]


def test_bios_selects_real_rtl_tx_without_losing_emulator_batching() -> None:
    """Pin the closed software/RTL seam without claiming attached hardware."""

    architecture = _read(ROOT / "docs" / "architecture.md")
    host_port = _read(RICH_DOCS / "MEGAPAD-TERMINAL-HOST-PORT.md")
    bios = _read(ROOT / "bios.asm")
    guest = _read(ROOT / "rich-terminal.f")
    rtl_uart = _read(ROOT / "rtl" / "periph" / "mp64_uart.v")
    rtl_pkg = _read(ROOT / "rtl" / "pkg" / "mp64_pkg.vh")
    rtl_soc = _read(ROOT / "rtl" / "soc" / "mp64_soc.v")

    assert "0xFFFF_FF00_0000_0007  ; UART_CAPS" in bios
    assert "var_uart_caps" in bios
    assert "uart_write_direct:" in bios
    assert "ring_write_batch:" in bios
    assert "0xFFFF_FF00_0000_0000  ; UART_TX_DATA" in bios
    assert "0xFFFF_FF00_0000_0002  ; UART_STATUS" in bios
    assert "txf_direct:" in bios
    assert "ldi64 r11, ring_write" in bios
    assert bios.count("ldi64 r11, ring_write") == 7
    for direct_caller in ("print_str:", "print_hex_byte:", "print_crlf:"):
        caller = bios[bios.index(direct_caller):]
        caller = caller[:caller.index("ret.l") + len("ret.l")]
        assert "ldi64 r11, ring_write" in caller
    assert "0xFFFF_FF00_0000_0006" in bios
    assert "0xFFFF_FF00_0000_0008" in bios
    assert "TYPE TX-FLUSH" in guest

    assert "UART_CAPS" in rtl_pkg
    assert "UART_CAP_TX_RING_BATCH" in rtl_pkg
    assert "addr == UART_TX" in rtl_uart
    assert "UART_CAPS:    rdata <= 8'd0" in rtl_uart
    assert "wire tx_line_idle = tx_fifo_empty && !tx_active" in rtl_uart
    assert "case ({tx_push, tx_pop})" in rtl_uart
    assert ".BAUD_RATE (115200)" in rtl_soc

    status = "Software-to-RTL UART TX path is **IMPLEMENTED; BOARD EVIDENCE OPEN**"
    assert status in architecture
    assert status in host_port
    assert "does not prove attached-board" in host_port


def test_dual_rate_is_sequenced_after_real_tx_and_real_line_idle() -> None:
    """Do not let inert emulator baud bytes masquerade as dual-rate support."""

    architecture = _read(ROOT / "docs" / "architecture.md")
    wire = _read(RICH_DOCS / "APT-1-WIRE.md")
    python_uart = _read(ROOT / "emulator" / "devices.py")
    native_uart = _read(ROOT / "emulator" / "accel" / "mp64_uart.h")

    assert "tx_ready = 1       # always ready in emulator" in python_uart
    assert "self.baud_lo = value" in python_uart
    assert "self.baud_hi = value" in python_uart
    assert "return static_cast<uint8_t>(0x21" in native_uart
    assert "baud_lo = value" in native_uart
    assert "baud_hi = value" in native_uart

    assert "After attached-board physical TX is measured" in architecture
    assert "baseline 115,200 / fast 1,000,000 baud selector" in architecture
    assert "FIFO-and-shifter-idle boundary" in architecture
    assert "contains no baud-rate field" in wire
    assert "Link reset always restores 115,200" in wire
