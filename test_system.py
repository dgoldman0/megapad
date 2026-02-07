#!/usr/bin/env python3
"""
Integration tests for the Megapad-64 system emulator.

Tests the full stack: CPU + Devices + MMIO + BIOS + CLI integration.
"""
import os
import sys
import tempfile
import unittest

from megapad64 import Megapad64, HaltError
from asm import assemble, AsmError
from system import MegapadSystem, MMIO_START
from devices import (
    MMIO_BASE, UART_BASE, TIMER_BASE, STORAGE_BASE, SYSINFO_BASE,
    SECTOR_SIZE, UART, Timer, Storage, SystemInfo, DeviceBus,
)


# ---------------------------------------------------------------------------
#  Helpers
# ---------------------------------------------------------------------------

BIOS_PATH = os.path.join(os.path.dirname(__file__), "bios.asm")


def make_system(ram_kib: int = 256, storage_image: str = None) -> MegapadSystem:
    return MegapadSystem(ram_size=ram_kib * 1024, storage_image=storage_image)


def run_until(sys: MegapadSystem, max_steps: int = 500_000):
    """Run until halted or step limit."""
    for i in range(max_steps):
        sys.step()
        if sys.cpu.halted:
            return i
    return max_steps


def capture_uart(sys: MegapadSystem):
    """Set up UART output capture, return list ref."""
    buf = []
    sys.uart.on_tx = lambda b: buf.append(b)
    return buf


def uart_text(buf: list[int]) -> str:
    """Convert UART byte buffer to printable text."""
    return "".join(
        chr(b) if (0x20 <= b < 0x7F or b in (10, 13, 9)) else ""
        for b in buf
    )


# ---------------------------------------------------------------------------
#  Device-level tests
# ---------------------------------------------------------------------------

class TestUART(unittest.TestCase):
    def test_tx_callback(self):
        uart = UART()
        out = []
        uart.on_tx = lambda b: out.append(b)
        uart.write8(0x00, 0x41)  # TX 'A'
        uart.write8(0x00, 0x42)  # TX 'B'
        self.assertEqual(out, [0x41, 0x42])

    def test_rx_inject(self):
        uart = UART()
        uart.inject_input(b"Hi")
        # STATUS should show RX available
        status = uart.read8(0x02)
        self.assertTrue(status & 0x02)
        # Read two bytes
        self.assertEqual(uart.read8(0x01), ord("H"))
        self.assertEqual(uart.read8(0x01), ord("i"))
        # Now empty
        status = uart.read8(0x02)
        self.assertFalse(status & 0x02)

    def test_tx_drain(self):
        uart = UART()
        uart.write8(0x00, 0x41)
        uart.write8(0x00, 0x42)
        self.assertEqual(uart.drain_tx(), "AB")  # drain_tx returns str
        self.assertEqual(uart.drain_tx(), "")


class TestTimer(unittest.TestCase):
    def test_tick_count(self):
        timer = Timer()
        # Enable timer: CONTROL is at offset 0x08
        timer.write8(0x08, 0x01)  # CTRL bit 0 = enable
        for _ in range(100):
            timer.tick(1)
        # Read counter (32-bit, little-endian at offset 0)
        lo = timer.read8(0x00)
        hi = timer.read8(0x01)
        count = lo | (hi << 8)
        self.assertEqual(count, 100)


class TestStorage(unittest.TestCase):
    def test_create_and_rw(self):
        with tempfile.NamedTemporaryFile(suffix=".img", delete=False) as f:
            path = f.name
        try:
            stor = Storage()
            # Remove the temp so load_image creates a fresh image
            os.unlink(path)
            stor.load_image(path)  # creates default 1 MiB image
            self.assertGreater(stor.total_sectors, 0)
            self.assertTrue(stor.status & 0x80)

            # Write test data into internal buffer
            test_data = bytes(range(256))
            stor._image_data[:256] = test_data
            self.assertEqual(stor._image_data[:10], bytes(range(10)))

            # Save and reload
            stor.save_image()
            stor2 = Storage()
            stor2.load_image(path)
            self.assertEqual(stor2._image_data[:10], bytes(range(10)))
        finally:
            if os.path.exists(path):
                os.unlink(path)

    def test_no_image(self):
        stor = Storage()
        self.assertFalse(stor.status & 0x80)


class TestDeviceBus(unittest.TestCase):
    def test_route_uart(self):
        bus = DeviceBus()
        uart = UART()
        bus.register(uart)
        out = []
        uart.on_tx = lambda b: out.append(b)
        # UART_BASE (0x0000) is already the MMIO aperture offset
        bus.write8(UART_BASE, 0x48)  # TX 'H'
        self.assertEqual(out, [0x48])

    def test_route_sysinfo(self):
        bus = DeviceBus()
        si = SystemInfo(mem_size_kib=256)
        bus.register(si)
        # SYSINFO_BASE (0x0300) is the MMIO aperture offset
        self.assertEqual(bus.read8(SYSINFO_BASE + 0), ord("M"))
        self.assertEqual(bus.read8(SYSINFO_BASE + 1), ord("P"))
        self.assertEqual(bus.read8(SYSINFO_BASE + 2), ord("6"))
        self.assertEqual(bus.read8(SYSINFO_BASE + 3), ord("4"))


# ---------------------------------------------------------------------------
#  System / MMIO tests
# ---------------------------------------------------------------------------

class TestSystemMMIO(unittest.TestCase):
    def test_uart_tx_via_cpu(self):
        """CPU writing to UART MMIO address should trigger TX."""
        sys = make_system()
        buf = capture_uart(sys)
        # Full MMIO address = MMIO_BASE + UART_BASE + 0
        uart_addr = MMIO_BASE + UART_BASE
        code = assemble(f"""
            ldi64 r1, {uart_addr}
            ldi r2, 0x41
            st.b r1, r2
            halt
        """)
        sys.load_binary(0, code)
        sys.boot()
        run_until(sys)
        self.assertTrue(sys.cpu.halted)
        self.assertEqual(buf, [0x41])

    def test_sysinfo_read_via_cpu(self):
        """CPU reading SysInfo board ID through MMIO."""
        sys = make_system(ram_kib=64)
        sysinfo_addr = MMIO_BASE + SYSINFO_BASE
        code = assemble(f"""
            ldi64 r1, {sysinfo_addr}
            ld.b r4, r1
            inc r1
            ld.b r5, r1
            halt
        """)
        sys.load_binary(0, code)
        sys.boot()
        run_until(sys)
        self.assertTrue(sys.cpu.halted)
        self.assertEqual(sys.cpu.regs[4], ord("M"))
        self.assertEqual(sys.cpu.regs[5], ord("P"))

    def test_normal_ram_unaffected(self):
        """Non-MMIO addresses should read/write RAM normally."""
        sys = make_system()
        code = assemble("""
            ldi r1, 0xAA
            ldi64 r4, 0x8000
            st.b r4, r1
            ldi r5, 0
            ld.b r5, r4
            halt
        """)
        sys.load_binary(0, code)
        sys.boot()
        run_until(sys)
        self.assertTrue(sys.cpu.halted)
        self.assertEqual(sys.cpu.regs[5], 0xAA)


# ---------------------------------------------------------------------------
#  BIOS tests
# ---------------------------------------------------------------------------

class TestBIOS(unittest.TestCase):

    def setUp(self):
        with open(BIOS_PATH) as f:
            self.bios_code = assemble(f.read())

    def _boot_bios(self, ram_kib=256, storage_image=None):
        sys = make_system(ram_kib=ram_kib, storage_image=storage_image)
        buf = capture_uart(sys)
        sys.load_binary(0, self.bios_code)
        sys.boot()
        return sys, buf

    def _run_to_prompt(self, sys, buf, max_steps=500_000):
        run_until(sys, max_steps)
        text = uart_text(buf)
        buf.clear()
        return text

    def _send_cmd(self, sys, buf, cmd_bytes: bytes, max_steps=500_000):
        sys.uart.inject_input(cmd_bytes)
        run_until(sys, max_steps)
        text = uart_text(buf)
        buf.clear()
        return text

    def test_boot_banner(self):
        sys, buf = self._boot_bios()
        text = self._run_to_prompt(sys, buf)
        self.assertIn("Megapad-64 BIOS v0.2", text)
        self.assertIn("Storage: N", text)
        self.assertIn("Ready.", text)
        self.assertIn("> ", text)

    def test_boot_with_storage(self):
        with tempfile.NamedTemporaryFile(suffix=".img", delete=False) as f:
            path = f.name
            f.write(b"\x00" * SECTOR_SIZE * 4)
        try:
            sys, buf = self._boot_bios(storage_image=path)
            text = self._run_to_prompt(sys, buf)
            self.assertIn("Storage: Y", text)
        finally:
            os.unlink(path)

    def test_help_command(self):
        sys, buf = self._boot_bios()
        self._run_to_prompt(sys, buf)
        text = self._send_cmd(sys, buf, b"h")
        self.assertIn("h=help", text)
        self.assertIn("q=quit", text)
        self.assertIn("> ", text)

    def test_regs_command(self):
        sys, buf = self._boot_bios()
        self._run_to_prompt(sys, buf)
        text = self._send_cmd(sys, buf, b"r")
        self.assertIn("PC=", text)
        self.assertIn("R9=00", text)
        self.assertIn("> ", text)

    def test_dump_command(self):
        sys, buf = self._boot_bios()
        self._run_to_prompt(sys, buf)
        text = self._send_cmd(sys, buf, b"d")
        # The first byte of BIOS is 0x78 (MOV R15, R2)
        self.assertIn("78", text)
        self.assertIn("> ", text)

    def test_setaddr_then_dump(self):
        sys, buf = self._boot_bios()
        self._run_to_prompt(sys, buf)
        # Set address to 0x10 (4 hex digits in v0.2)
        self._send_cmd(sys, buf, b"s0010")
        self.assertEqual(sys.cpu.regs[9], 0x10)
        # Dump from 0x10
        text = self._send_cmd(sys, buf, b"d")
        parts = text.replace("\n", " ").replace("\r", " ").split()
        hex_count = sum(1 for p in parts if len(p) == 2
                       and all(c in "0123456789ABCDEFabcdef" for c in p))
        self.assertGreaterEqual(hex_count, 16)

    def test_quit_command(self):
        sys, buf = self._boot_bios()
        self._run_to_prompt(sys, buf)
        text = self._send_cmd(sys, buf, b"q")
        self.assertIn("Bye!", text)
        self.assertTrue(sys.cpu.halted)

    def test_unknown_command(self):
        sys, buf = self._boot_bios()
        self._run_to_prompt(sys, buf)
        text = self._send_cmd(sys, buf, b"Z")
        self.assertIn("?", text)
        self.assertIn("> ", text)

    def test_multiple_commands(self):
        """Run several commands in sequence."""
        sys, buf = self._boot_bios()
        self._run_to_prompt(sys, buf)

        # Help
        text = self._send_cmd(sys, buf, b"h")
        self.assertIn("h=help", text)

        # Set address (4 hex digits in v0.2)
        self._send_cmd(sys, buf, b"s0020")
        self.assertEqual(sys.cpu.regs[9], 0x20)

        # Dump
        text = self._send_cmd(sys, buf, b"d")
        self.assertIn("> ", text)

        # Regs (R9 shown as hex32 in v0.2)
        text = self._send_cmd(sys, buf, b"r")
        self.assertIn("R9=00000020", text)

        # Quit
        text = self._send_cmd(sys, buf, b"q")
        self.assertTrue(sys.cpu.halted)


# ---------------------------------------------------------------------------
#  Assembler branch-range test
# ---------------------------------------------------------------------------

class TestAssemblerBranchRange(unittest.TestCase):
    def test_short_branch_in_range(self):
        code = assemble("loop:\n  br loop")
        self.assertEqual(len(code), 2)

    def test_short_branch_out_of_range(self):
        lines = ["target:\n"] + ["nop\n"] * 200 + ["br target\n"]
        with self.assertRaises(AsmError):
            assemble("".join(lines))

    def test_long_branch_far(self):
        lines = ["target:\n"] + ["nop\n"] * 200 + ["lbr target\n"]
        code = assemble("".join(lines))
        self.assertEqual(len(code), 200 + 3)


# ---------------------------------------------------------------------------
#  Main
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    print("=" * 60)
    print("  Megapad-64 System Integration Tests")
    print("=" * 60)

    loader = unittest.TestLoader()
    suite = loader.loadTestsFromModule(sys.modules[__name__])

    passed = 0
    failed = 0

    for test_group in suite:
        for test in test_group:
            result = unittest.TestResult()
            test.run(result)
            name = str(test)
            if result.wasSuccessful():
                passed += 1
                print(f"  ✓ {name}")
            else:
                failed += 1
                for _, tb in result.failures + result.errors:
                    last_line = tb.strip().split("\n")[-1]
                    print(f"  ✗ {name}: {last_line}")

    print()
    print("=" * 60)
    total = passed + failed
    print(f"  Results: {passed} passed, {failed} failed (of {total})")
    print("=" * 60)
    sys.exit(1 if failed else 0)
