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
    NIC_BASE, SECTOR_SIZE, UART, Timer, Storage, SystemInfo,
    NetworkDevice, DeviceBus,
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


class TestNIC(unittest.TestCase):
    """Tests for the NetworkDevice (NIC) peripheral."""

    def test_present_and_link_up(self):
        nic = NetworkDevice()
        status = nic.read8(0x01)
        self.assertTrue(status & 0x80)   # present
        self.assertTrue(status & 0x04)   # link up
        self.assertFalse(status & 0x02)  # no RX pending

    def test_mac_address(self):
        mac = b'\x02\x4D\x50\x36\x34\x00'
        nic = NetworkDevice(mac=mac)
        for i, b in enumerate(mac):
            self.assertEqual(nic.read8(0x0E + i), b)

    def test_inject_and_recv_frame(self):
        nic = NetworkDevice()
        frame = b'Hello NIC!'
        nic.inject_frame(frame)
        # STATUS should show RX available
        self.assertTrue(nic.read8(0x01) & 0x02)
        self.assertEqual(nic.rx_count, 1)
        # Set up data port recv
        nic._execute_cmd(0x02)  # RECV
        self.assertEqual(nic.frame_len, len(frame))
        # Read via data port
        result = bytearray()
        for _ in range(nic.frame_len):
            result.append(nic.read8(0x20))  # DATA port
        self.assertEqual(result, frame)
        # RX queue empty now
        self.assertFalse(nic.read8(0x01) & 0x02)

    def test_tx_via_data_port(self):
        nic = NetworkDevice()
        sent = []
        nic.on_tx_frame = lambda f: sent.append(f)
        # Write frame via data port
        frame = b'TX test'
        for b in frame:
            nic.write8(0x20, b)
        # Set frame length
        nic.write8(0x0A, len(frame) & 0xFF)
        nic.write8(0x0B, 0)
        # Send
        nic._execute_cmd(0x01)
        self.assertEqual(len(sent), 1)
        self.assertEqual(sent[0], frame)
        self.assertEqual(nic.tx_count, 1)

    def test_tx_via_dma(self):
        """TX using DMA with mem_read callback."""
        nic = NetworkDevice()
        sent = []
        nic.on_tx_frame = lambda f: sent.append(f)
        # Simulate RAM with a simple dict
        ram = bytearray(256)
        msg = b'DMA frame!'
        ram[:len(msg)] = msg
        nic._mem_read = lambda addr: ram[addr % len(ram)]
        # Set DMA addr = 0, frame_len = len(msg)
        for i in range(8):
            nic.write8(0x02 + i, 0)  # DMA addr = 0
        nic.write8(0x0A, len(msg) & 0xFF)
        nic.write8(0x0B, 0)
        nic._execute_cmd(0x01)
        self.assertEqual(sent[0], msg)

    def test_rx_via_dma(self):
        """RX using DMA with mem_write callback."""
        nic = NetworkDevice()
        ram = bytearray(256)
        nic._mem_write = lambda addr, val: ram.__setitem__(addr % len(ram), val & 0xFF)
        frame = b'incoming'
        nic.inject_frame(frame)
        # Set DMA addr = 0x10
        nic.write8(0x02, 0x10)
        for i in range(1, 8):
            nic.write8(0x02 + i, 0)
        nic._execute_cmd(0x02)  # RECV
        self.assertEqual(nic.frame_len, len(frame))
        self.assertEqual(bytes(ram[0x10:0x10 + len(frame)]), frame)

    def test_reset_clears_state(self):
        nic = NetworkDevice()
        nic.inject_frame(b'test')
        nic._execute_cmd(0x04)  # RESET
        self.assertEqual(len(nic.rx_queue), 0)
        self.assertEqual(nic.tx_count, 0)
        self.assertEqual(nic.rx_count, 0)
        self.assertEqual(nic.frame_len, 0)

    def test_counters(self):
        nic = NetworkDevice()
        sent = []
        nic.on_tx_frame = lambda f: sent.append(f)
        for i in range(3):
            nic.inject_frame(bytes([i]))
        self.assertEqual(nic.rx_count, 3)
        # Send two frames via data port
        for i in range(2):
            nic._data_buf = bytearray([0x41 + i])
            nic.frame_len = 1
            nic._execute_cmd(0x01)
        self.assertEqual(nic.tx_count, 2)
        # Read counters via register
        self.assertEqual(nic.read8(0x14), 2)  # TX_COUNT_LO
        self.assertEqual(nic.read8(0x16), 3)  # RX_COUNT_LO

    def test_sysinfo_nic_present(self):
        si = SystemInfo(has_nic=True)
        self.assertEqual(si.read8(0x0A), 1)
        si2 = SystemInfo(has_nic=False)
        self.assertEqual(si2.read8(0x0A), 0)

    def test_bus_routes_nic(self):
        bus = DeviceBus()
        nic = NetworkDevice()
        bus.register(nic)
        # Read STATUS via bus at NIC_BASE offset
        status = bus.read8(NIC_BASE + 0x01)
        self.assertTrue(status & 0x80)  # present

    def test_mtu_truncation(self):
        nic = NetworkDevice()
        big_frame = bytes(2000)
        nic.inject_frame(big_frame)
        self.assertEqual(len(nic.rx_queue[0]), 1500)


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

    def _run_forth(self, sys, buf, input_lines: list[str],
                   max_steps=2_000_000) -> str:
        """Feed newline-terminated Forth commands and collect UART output.

        Sends input one byte at a time with CPU execution between bytes
        (matching _console_pipe behaviour).  Always appends BYE to ensure
        the CPU halts.
        """
        payload = "\n".join(input_lines) + "\nBYE\n"
        data = payload.encode()
        pos = 0

        for _ in range(len(data) + 20):          # outer byte-feed loop
            # Run CPU until idle (waiting for UART) or halt
            for _ in range(max_steps // (len(data) + 20)):
                if sys.cpu.halted:
                    break
                if sys.cpu.idle and not sys.uart.has_rx_data:
                    break
                try:
                    sys.step()
                except HaltError:
                    break
                except Exception:
                    break
            if sys.cpu.halted:
                break
            if pos < len(data):
                sys.uart.inject_input(bytes([data[pos]]))
                pos += 1
            else:
                break

        return uart_text(buf)

    # -- Banner --

    def test_boot_banner(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, [])
        self.assertIn("Megapad-64 Forth BIOS v0.3", text)
        self.assertIn("RAM:", text)
        self.assertIn("ok", text)

    # -- Basic number printing --

    def test_print_zero(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["0 ."])
        self.assertIn("0 ", text)

    def test_print_positive(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["42 ."])
        self.assertIn("42 ", text)

    def test_print_negative(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["-7 ."])
        self.assertIn("-7 ", text)

    def test_hex_literal(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["0xFF ."])
        self.assertIn("255 ", text)

    # -- Arithmetic --

    def test_add(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["3 4 + ."])
        self.assertIn("7 ", text)

    def test_subtract(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["10 3 - ."])
        self.assertIn("7 ", text)

    def test_multiply(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["6 7 * ."])
        self.assertIn("42 ", text)

    def test_divmod(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["100 7 /MOD . ."])
        self.assertIn("14 2 ", text)

    # -- Stack operations --

    def test_dup(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["5 DUP . ."])
        self.assertIn("5 5 ", text)

    def test_swap(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["1 2 SWAP . ."])
        self.assertIn("1 2 ", text)

    def test_drop(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["1 2 DROP ."])
        self.assertIn("1 ", text)

    def test_dot_s(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["1 2 3 .S", "DROP DROP DROP"])
        self.assertIn("<3>", text)
        self.assertIn("1", text)

    def test_depth(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["42 DEPTH ."])
        self.assertIn("1 ", text)

    # -- Comparison & logic --

    def test_less_than(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["3 5 < ."])
        self.assertIn("-1 ", text)

    def test_greater_than(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["5 3 > ."])
        self.assertIn("-1 ", text)

    def test_equal(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["7 7 = ."])
        self.assertIn("-1 ", text)

    def test_and(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["0xFF 0x0F AND ."])
        self.assertIn("15 ", text)

    def test_or(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["0xF0 0x0F OR ."])
        self.assertIn("255 ", text)

    # -- Memory --

    def test_store_fetch(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["42 0x2000 !", "0x2000 @ ."])
        self.assertIn("42 ", text)

    def test_c_store_c_fetch(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["0xAB 0x3000 C!", "0x3000 C@ ."])
        self.assertIn("171 ", text)

    def test_fill_dump(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["0x2000 16 0xCC FILL", "0x2000 16 DUMP"])
        self.assertIn("CC", text)

    # -- I/O --

    def test_emit(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["65 EMIT"])
        self.assertIn("A", text)

    # -- HEX / DECIMAL / BASE --

    def test_hex_mode(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["HEX FF . DECIMAL"])
        self.assertIn("FF ", text)

    def test_base_change(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["HEX CAFE . DECIMAL"])
        self.assertIn("CAFE ", text)

    # -- WORDS --

    def test_words(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["WORDS"])
        self.assertIn("DUP", text)
        self.assertIn("DROP", text)
        self.assertIn("SWAP", text)
        self.assertIn("WORDS", text)

    # -- BYE --

    def test_bye(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, [])  # just BYE appended by helper
        self.assertIn("Bye!", text)
        self.assertTrue(sys.cpu.halted)

    # -- Undefined word --

    def test_undefined_word(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["NOSUCHWORD"])
        self.assertIn("NOSUCHWORD", text)
        self.assertIn("?", text)


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
