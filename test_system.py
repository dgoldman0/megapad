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
        self.assertIn("Megapad-64 Forth BIOS v0.4", text)
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

    # -- v0.4: Colon definitions --

    def test_colon_def_simple(self):
        """Define and call a simple word."""
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, [
            ": DOUBLE DUP + ;",
            "21 DOUBLE .",
        ])
        self.assertIn("42 ", text)

    def test_colon_def_nested(self):
        """Words calling other user-defined words."""
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, [
            ": SQ DUP * ;",
            ": CUBE DUP SQ * ;",
            "3 CUBE .",
        ])
        self.assertIn("27 ", text)

    # -- v0.4: IF / ELSE / THEN --

    def test_if_then_true(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, [
            ": TEST1 1 IF 42 . THEN ;",
            "TEST1",
        ])
        self.assertIn("42 ", text)

    def test_if_then_false(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, [
            ": TEST2 0 IF 99 . THEN 7 . ;",
            "TEST2",
        ])
        # Extract output after "TEST2\r\n" to avoid matching "99" in the echo
        idx = text.rfind("TEST2")
        after = text[idx:] if idx >= 0 else text
        self.assertNotIn("99 ", after)
        self.assertIn("7 ", after)

    def test_if_else_then(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, [
            ": ABS2 DUP 0 < IF 0 SWAP - ELSE THEN ;",
            "-5 ABS2 .",
            "3 ABS2 .",
        ])
        self.assertIn("5 ", text)
        self.assertIn("3 ", text)

    # -- v0.4: BEGIN / UNTIL --

    def test_begin_until(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, [
            ": COUNT5 0 BEGIN 1 + DUP 5 = UNTIL . ;",
            "COUNT5",
        ])
        self.assertIn("5 ", text)

    # -- v0.4: DO / LOOP / I --

    def test_do_loop(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, [
            ": SHOW3 3 0 DO I . LOOP ;",
            "SHOW3",
        ])
        self.assertIn("0 ", text)
        self.assertIn("1 ", text)
        self.assertIn("2 ", text)

    # -- v0.4: VARIABLE --

    def test_variable(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, [
            "VARIABLE X",
            "42 X !",
            "X @ .",
        ])
        self.assertIn("42 ", text)

    # -- v0.4: CONSTANT --

    def test_constant(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, [
            "99 CONSTANT LIMIT",
            "LIMIT .",
        ])
        # Ensure the value printed by LIMIT . is 99 (not just echo)
        self.assertIn("99 ", text.split("LIMIT .")[1])

    # -- v0.4: TYPE / SPACE / SPACES --

    def test_space_word(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, [
            "65 EMIT SPACE 66 EMIT",
        ])
        self.assertIn("A B", text)

    # -- v0.4: ." (dot-quote) --

    def test_dotquote(self):
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, [
            ': HELLO ." Hello World" ;',
            "HELLO",
        ])
        self.assertIn("Hello World", text)

    # -- v0.4: NIC words --

    def test_net_status(self):
        """NET-STATUS should return NIC status byte (present|link)."""
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["NET-STATUS ."])
        # NIC present=bit7(0x80), link=bit2(0x04) → 0x84 = 132
        self.assertIn("132 ", text)

    def test_net_mac(self):
        """NET-MAC@ should push a valid MMIO address."""
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["NET-MAC@ C@ ."])
        # First byte of default MAC 02:00:00:00:00:01
        self.assertIn("2 ", text)

    def test_words_includes_new(self):
        """WORDS should list the new v0.4 words."""
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["WORDS"])
        for word in ["VARIABLE", "CONSTANT", "IF", "THEN", "ELSE",
                     "DO", "LOOP", "BEGIN", "UNTIL", "NET-STATUS",
                     "SPACE", "TYPE", "DISK-READ", "DISK-WRITE"]:
            self.assertIn(word, text, f"Missing word: {word}")

    # -- Storage words --

    def test_disk_status_no_image(self):
        """DISK@ returns 0 when no storage image is attached."""
        sys, buf = self._boot_bios()
        text = self._run_forth(sys, buf, ["DISK@ ."])
        self.assertIn("0 ", text)

    def test_disk_status_with_image(self):
        """DISK@ returns non-zero (bit7=present) when image attached."""
        with tempfile.NamedTemporaryFile(suffix=".img", delete=False) as f:
            path = f.name
        try:
            os.unlink(path)
            sys, buf = self._boot_bios(storage_image=path)
            text = self._run_forth(sys, buf, ["DISK@ ."])
            # bit 7 = 0x80 = 128
            self.assertIn("128 ", text)
        finally:
            if os.path.exists(path):
                os.unlink(path)

    def test_disk_write_read_roundtrip(self):
        """Write data to disk sector 0, read it back via DMA."""
        with tempfile.NamedTemporaryFile(suffix=".img", delete=False) as f:
            path = f.name
        try:
            os.unlink(path)
            sys, buf = self._boot_bios(storage_image=path)
            text = self._run_forth(sys, buf, [
                "VARIABLE dbuf  512 ALLOT",
                # Write 0xAA to first byte of our buffer
                "0xAA dbuf C!",
                # Set up DMA: sector 0, 1 sector, from dbuf address
                "0 DISK-SEC!",
                "dbuf DISK-DMA!",
                "1 DISK-N!",
                "DISK-WRITE",
                # Zero our buffer
                "0 dbuf C!",
                # Read it back
                "DISK-READ",
                # Check first byte
                "dbuf C@ .",
            ])
            self.assertIn("170 ", text)  # 0xAA = 170
        finally:
            if os.path.exists(path):
                os.unlink(path)


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
#  KDOS tests
# ---------------------------------------------------------------------------

KDOS_PATH = os.path.join(os.path.dirname(__file__), "kdos.f")


class TestKDOS(unittest.TestCase):
    """Integration tests for KDOS — Kernel Dashboard OS.

    Loads kdos.f via UART injection, then sends additional commands
    and checks output.
    """

    def setUp(self):
        with open(BIOS_PATH) as f:
            self.bios_code = assemble(f.read())
        with open(KDOS_PATH) as f:
            self.kdos_lines = []
            for line in f.read().splitlines():
                stripped = line.strip()
                if not stripped:
                    continue
                # Skip pure comment lines to reduce UART byte count.
                # Lines starting with \ are backslash comments.
                if stripped.startswith('\\'):
                    continue
                self.kdos_lines.append(line)

    def _boot_bios(self, ram_kib=256, storage_image=None):
        sys = make_system(ram_kib=ram_kib, storage_image=storage_image)
        buf = capture_uart(sys)
        sys.load_binary(0, self.bios_code)
        sys.boot()
        return sys, buf

    def _run_kdos(self, extra_lines: list[str],
                  max_steps=400_000_000,
                  storage_image=None) -> str:
        """Load KDOS then execute extra_lines, return UART output."""
        sys, buf = self._boot_bios(storage_image=storage_image)
        all_lines = self.kdos_lines + extra_lines
        payload = "\n".join(all_lines) + "\nBYE\n"
        data = payload.encode()
        pos = 0
        chunk = max_steps // (len(data) + 20)

        for _ in range(len(data) + 20):
            for _ in range(chunk):
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

    # -- Loading --

    def test_kdos_loads(self):
        """KDOS loads without errors and prints banner."""
        text = self._run_kdos([])
        self.assertIn("KDOS v0.4", text)
        self.assertIn("HELP", text)

    # -- Utility words --

    def test_cells(self):
        text = self._run_kdos(["3 CELLS ."])
        self.assertIn("24 ", text)

    def test_min_max(self):
        text = self._run_kdos(["3 7 MIN .", "3 7 MAX ."])
        self.assertIn("3 ", text)
        self.assertIn("7 ", text)

    # -- Buffers --

    def test_buffer_create(self):
        """Create a buffer and query its fields."""
        text = self._run_kdos([
            "0 1 128 BUFFER test-buf",
            "test-buf B.TYPE .",
            "test-buf B.WIDTH .",
            "test-buf B.LEN .",
        ])
        # Extract output after KDOS banner loaded
        idx = text.rfind("DASHBOARD")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("0 ", out)   # type = 0 (raw)
        self.assertIn("1 ", out)   # width = 1
        self.assertIn("128 ", out) # length = 128

    def test_buffer_tiles(self):
        """B.TILES returns correct tile count."""
        text = self._run_kdos([
            "0 1 128 BUFFER tb",
            "tb B.TILES .",
        ])
        idx = text.rfind("DASHBOARD")
        out = text[idx:] if idx >= 0 else text
        # 128 bytes / 64 = 2 tiles
        self.assertIn("2 ", out)

    def test_buffer_zero_and_preview(self):
        """B.ZERO fills with zeros, B.PREVIEW shows them."""
        text = self._run_kdos([
            "0 1 64 BUFFER zb",
            "zb B.ZERO",
            "zb B.PREVIEW",
        ])
        # All bytes should be 0
        idx = text.rfind("B.PREVIEW")
        out = text[idx:] if idx >= 0 else text
        # Should see lots of "0 " in the preview output
        self.assertTrue(out.count("0 ") >= 16)

    def test_buffer_fill(self):
        """B.FILL fills with specified byte."""
        text = self._run_kdos([
            "0 1 64 BUFFER fb",
            "42 fb B.FILL",
            "fb B.DATA C@ .",
        ])
        idx = text.rfind("DASHBOARD")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("42 ", out)

    def test_buffers_list(self):
        """BUFFERS lists registered buffers."""
        text = self._run_kdos([
            "0 1 64 BUFFER b1",
            "2 8 16 BUFFER b2",
            "BUFFERS",
        ])
        # Should show count and info for both + the kzero built-in
        self.assertIn("Buffers", text)

    def test_buffer_info(self):
        """B.INFO prints descriptor details."""
        text = self._run_kdos([
            "0 1 256 BUFFER ib",
            "ib B.INFO",
        ])
        self.assertIn("[buf", text)
        self.assertIn("t=", text)
        self.assertIn("w=", text)
        self.assertIn("n=", text)

    # -- Kernels --

    def test_kernel_register(self):
        """Register a kernel and query its descriptor."""
        text = self._run_kdos([
            "1 1 2 0 KERNEL test-kern",
            "test-kern K.IN .",
            "test-kern K.OUT .",
            "test-kern K.FOOT .",
            "test-kern K.FLAGS .",
        ])
        idx = text.rfind("DASHBOARD")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("1 ", out)   # n_in = 1
        self.assertIn("2 ", out)   # footprint = 2
        self.assertIn("0 ", out)   # flags = 0

    def test_kernels_list(self):
        """KERNELS lists registered kernels including built-in."""
        text = self._run_kdos(["KERNELS"])
        self.assertIn("Kernels", text)
        # kzero-desc is registered by kdos.f
        self.assertIn("[kern", text)

    def test_kernel_info(self):
        """K.INFO prints kernel descriptor."""
        text = self._run_kdos([
            "2 1 4 1 KERNEL ki-test",
            "ki-test K.INFO",
        ])
        self.assertIn("[kern", text)
        self.assertIn("in=", text)
        self.assertIn("out=", text)
        self.assertIn("foot=", text)

    # -- Dashboard --

    def test_dashboard(self):
        """DASHBOARD prints the full system overview."""
        text = self._run_kdos([
            "0 1 64 BUFFER db",
            "DASHBOARD",
        ])
        self.assertIn("KDOS v0.4", text)
        self.assertIn("HERE", text)
        self.assertIn("Buffers", text)
        self.assertIn("Kernels", text)
        self.assertIn("Pipelines", text)
        self.assertIn("Storage", text)
        self.assertIn("Files", text)

    # -- Pipelines --

    def test_pipeline_create(self):
        """Create a pipeline and check its capacity."""
        text = self._run_kdos([
            "4 PIPELINE tp",
            "tp P.CAP .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("4 ", out)

    def test_pipeline_add_count(self):
        """P.ADD increments pipeline step count."""
        text = self._run_kdos([
            ": ts1 ;",
            ": ts2 ;",
            "3 PIPELINE tp2",
            "' ts1 tp2 P.ADD",
            "' ts2 tp2 P.ADD",
            "tp2 P.COUNT .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("2 ", out)

    def test_pipeline_run(self):
        """P.RUN executes all pipeline steps in order."""
        text = self._run_kdos([
            ": ps1 42 . ;",
            ": ps2 99 . ;",
            "2 PIPELINE tp3",
            "' ps1 tp3 P.ADD",
            "' ps2 tp3 P.ADD",
            "tp3 P.RUN",
        ])
        self.assertIn("42 ", text)
        self.assertIn("99 ", text)

    def test_pipeline_bench(self):
        """P.BENCH times each step and prints cycles."""
        text = self._run_kdos([
            ": pb1 ;",
            "1 PIPELINE tp4",
            "' pb1 tp4 P.ADD",
            "tp4 P.BENCH",
        ])
        self.assertIn("Pipeline", text)
        self.assertIn("cycles", text)

    def test_pipeline_info(self):
        """P.INFO shows pipeline descriptor."""
        text = self._run_kdos([
            "3 PIPELINE tp5",
            "tp5 P.INFO",
        ])
        self.assertIn("[pipe", text)
        self.assertIn("cap=", text)
        self.assertIn("steps=", text)

    def test_pipeline_clear(self):
        """P.CLEAR resets step count to 0."""
        text = self._run_kdos([
            ": pc1 ;",
            "2 PIPELINE tp6",
            "' pc1 tp6 P.ADD",
            "tp6 P.CLEAR",
            "tp6 P.COUNT .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("0 ", out)

    def test_empty_pipeline_run(self):
        """P.RUN on empty pipeline does nothing (no crash)."""
        text = self._run_kdos([
            "3 PIPELINE empty-pipe",
            "empty-pipe P.RUN",
            "42 .",
        ])
        self.assertIn("42 ", text)

    def test_pipes_list(self):
        """PIPES lists all registered pipelines."""
        text = self._run_kdos(["PIPES"])
        self.assertIn("Pipelines", text)
        self.assertIn("[pipe", text)

    def test_pipe_count(self):
        """3 sample pipelines are registered at startup."""
        text = self._run_kdos(["PIPE-COUNT @ ."])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("3 ", out)

    def test_pipe_fill_sum(self):
        """Built-in fill-sum pipeline: fill 64 bytes with 42, sum=2688."""
        text = self._run_kdos(["pipe-fill-sum P.RUN"])
        self.assertIn("sum=", text)
        self.assertIn("2688 ", text)

    def test_pipe_add_stats(self):
        """Built-in add-stats pipeline: 10+20=30, 64*30=1920."""
        text = self._run_kdos(["pipe-add-stats P.RUN"])
        self.assertIn("sum=", text)
        self.assertIn("1920 ", text)
        self.assertIn("max=", text)

    def test_pipe_thresh(self):
        """Built-in threshold pipeline: ramp 0..63, thresh at 32."""
        text = self._run_kdos(["pipe-thresh P.RUN"])
        # bytes 0-31 -> 0, bytes 32-63 -> 255, sum = 32*255 = 8160
        self.assertIn("sum=", text)
        self.assertIn("8160 ", text)

    def test_demo_buffers_exist(self):
        """Demo buffers demo-a, demo-b, demo-c are registered."""
        text = self._run_kdos([
            "demo-a B.LEN .",
            "demo-b B.LEN .",
            "demo-c B.LEN .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        count = out.count("64 ")
        self.assertTrue(count >= 3,
                        f"Expected >=3 occurrences of '64 ', got {count}")

    # -- Built-in kzero kernel --

    def test_kzero_kernel(self):
        """The built-in kzero word zeros a buffer."""
        text = self._run_kdos([
            "0 1 64 BUFFER kzb",
            "255 kzb B.FILL",
            "kzb B.DATA C@ .",
            "kzb kzero",
            "kzb B.DATA C@ .",
        ])
        idx = text.rfind("DASHBOARD")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("255 ", out)  # before zero
        # After kzero, first byte should be 0
        # Find the second number output after "255"
        pos255 = out.find("255 ")
        after = out[pos255 + 4:]
        self.assertIn("0 ", after)

    # -- Comment words --

    def test_backslash_comment(self):
        """Backslash comment skips rest of line."""
        text = self._run_kdos([
            "42 . \\ this is a comment and should not cause errors",
        ])
        self.assertIn("42 ", text)

    def test_paren_comment(self):
        """Paren comment skips to closing paren."""
        text = self._run_kdos([
            "99 ( this is a comment ) .",
        ])
        self.assertIn("99 ", text)

    # -- BIOS tile words --

    def test_acc_fetch(self):
        """ACC@ reads ACC0 CSR (initially 0)."""
        text = self._run_kdos(["ACC@ ."])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("0 ", out)

    def test_tmode_fetch(self):
        """TMODE@ reads current tile mode."""
        text = self._run_kdos(["TMODE@ ."])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("0 ", out)

    def test_execute_word(self):
        """EXECUTE calls a word given its XT."""
        text = self._run_kdos([
            ": test-word 777 . ;",
            "' test-word EXECUTE",
        ])
        self.assertIn("777 ", text)

    def test_tick_word(self):
        """' finds a word and returns nonzero XT."""
        text = self._run_kdos([
            "' DUP 0<> .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        # 0<> of nonzero XT should be -1 (true)
        self.assertIn("-1 ", out)

    # -- Tile-aware buffer ops --

    def test_bsum_zeros(self):
        """B.SUM of a zeroed buffer is 0."""
        text = self._run_kdos([
            "0 1 64 BUFFER sb",
            "sb B.ZERO",
            "sb B.SUM .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("0 ", out)

    def test_bsum_filled(self):
        """B.SUM of 64 bytes filled with 1 is 64."""
        text = self._run_kdos([
            "0 1 64 BUFFER sf",
            "1 sf B.FILL",
            "sf B.SUM .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("64 ", out)

    def test_bsum_multi_tile(self):
        """B.SUM of 128 bytes filled with 2 is 256."""
        text = self._run_kdos([
            "0 1 128 BUFFER sm",
            "2 sm B.FILL",
            "sm B.SUM .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("256 ", out)

    def test_bmin(self):
        """B.MIN finds the minimum byte."""
        text = self._run_kdos([
            "0 1 64 BUFFER mn",
            "100 mn B.FILL",
            "5 mn B.DATA C!",
            "mn B.MIN .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("5 ", out)

    def test_bmax(self):
        """B.MAX finds the maximum byte."""
        text = self._run_kdos([
            "0 1 64 BUFFER mx",
            "10 mx B.FILL",
            "200 mx B.DATA C!",
            "mx B.MAX .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("200 ", out)

    def test_badd(self):
        """B.ADD adds two buffers element-wise."""
        text = self._run_kdos([
            "0 1 64 BUFFER a1",
            "0 1 64 BUFFER a2",
            "0 1 64 BUFFER a3",
            "3 a1 B.FILL",
            "5 a2 B.FILL",
            "a1 a2 a3 B.ADD",
            "a3 B.DATA C@ .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("8 ", out)

    def test_bsub(self):
        """B.SUB subtracts two buffers element-wise."""
        text = self._run_kdos([
            "0 1 64 BUFFER s1",
            "0 1 64 BUFFER s2",
            "0 1 64 BUFFER s3",
            "10 s1 B.FILL",
            "3 s2 B.FILL",
            "s1 s2 s3 B.SUB",
            "s3 B.DATA C@ .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("7 ", out)

    def test_bscale(self):
        """B.SCALE multiplies each byte by n."""
        text = self._run_kdos([
            "0 1 64 BUFFER sc",
            "3 sc B.FILL",
            "4 sc B.SCALE",
            "sc B.DATA C@ .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("12 ", out)

    # -- Sample kernels --

    def test_kfill_kernel(self):
        """kfill fills a buffer with a byte."""
        text = self._run_kdos([
            "0 1 64 BUFFER kfb",
            "42 kfb kfill",
            "kfb B.DATA C@ .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("42 ", out)

    def test_kadd_kernel(self):
        """kadd adds two buffers."""
        text = self._run_kdos([
            "0 1 64 BUFFER ka1",
            "0 1 64 BUFFER ka2",
            "0 1 64 BUFFER ka3",
            "10 ka1 B.FILL",
            "20 ka2 B.FILL",
            "ka1 ka2 ka3 kadd",
            "ka3 B.DATA C@ .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("30 ", out)

    def test_ksum_kernel(self):
        """ksum sums all bytes in a buffer."""
        text = self._run_kdos([
            "0 1 64 BUFFER ksb",
            "1 ksb B.FILL",
            "ksb ksum .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("64 ", out)

    def test_kstats_kernel(self):
        """kstats returns sum, min, max."""
        text = self._run_kdos([
            "0 1 64 BUFFER ksb2",
            "5 ksb2 B.FILL",
            "10 ksb2 B.DATA C!",
            "1 ksb2 B.DATA 1+ C!",
            "ksb2 kstats",
            ". . .",
        ])
        # sum=5*62+10+1=321, min=1, max=10
        # . prints max first (TOS), then min, then sum
        self.assertIn("10 ", text)   # max
        self.assertIn("1 ", text)    # min

    def test_kscale_kernel(self):
        """kscale multiplies buffer by scalar."""
        text = self._run_kdos([
            "0 1 64 BUFFER kcb",
            "2 kcb B.FILL",
            "3 kcb kscale",
            "kcb B.DATA C@ .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("6 ", out)

    def test_kthresh_kernel(self):
        """kthresh sets bytes < threshold to 0, >= to 255."""
        text = self._run_kdos([
            "0 1 64 BUFFER ktb",
            "50 ktb B.FILL",
            "100 ktb B.DATA C!",
            "75 ktb kthresh",
            "ktb B.DATA C@ .",
            "ktb B.DATA 1+ C@ .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("255 ", out)   # 100 >= 75 → 255
        self.assertIn("0 ", out)     # 50 < 75 → 0

    # -- Benchmarking --

    def test_bench(self):
        """BENCH times a word and leaves cycles on stack."""
        text = self._run_kdos([
            ": noop-word ;",
            "' noop-word BENCH .",
        ])
        # Should print some nonzero cycle count
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        # Just check it doesn't error — any number is fine
        self.assertNotIn("?", out.split("BENCH")[-1] if "BENCH" in out else out[-100:])

    # -- Help & Status --

    def test_help(self):
        """HELP prints command reference."""
        text = self._run_kdos(["HELP"])
        self.assertIn("Quick Reference", text)
        self.assertIn("BUFFER WORDS", text)
        self.assertIn("KERNEL WORDS", text)
        self.assertIn("SAMPLE KERNELS", text)
        self.assertIn("PIPELINE WORDS", text)
        self.assertIn("STORAGE WORDS", text)
        self.assertIn("FILE WORDS", text)
        self.assertIn("BENCH", text)

    def test_status(self):
        """STATUS prints quick summary line."""
        text = self._run_kdos(["STATUS"])
        self.assertIn("KDOS", text)
        self.assertIn("bufs=", text)
        self.assertIn("kerns=", text)
        self.assertIn("pipes=", text)
        self.assertIn("files=", text)
        self.assertIn("disk=", text)

    # -- Kernel registry with new kernels --

    def test_kernel_count(self):
        """All sample kernels are registered."""
        text = self._run_kdos(["KERN-COUNT @ ."])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("7 ", out)  # kzero, kfill, kadd, ksum, kstats, kscale, kthresh

    # -- Utility words --

    def test_abs_word(self):
        """ABS computes absolute value."""
        text = self._run_kdos(["5 ABS .", "-3 ABS ."])
        self.assertIn("5 ", text)
        self.assertIn("3 ", text)

    def test_negate_word(self):
        """NEGATE negates a number."""
        text = self._run_kdos(["7 NEGATE ."])
        self.assertIn("-7 ", text)

    def test_2drop_word(self):
        """2DROP drops two items."""
        text = self._run_kdos(["1 2 3 2DROP ."])
        self.assertIn("1 ", text)

    def test_nip_word(self):
        """NIP removes second item."""
        text = self._run_kdos(["1 2 NIP ."])
        self.assertIn("2 ", text)

    # -- Storage & persistence --

    def test_disk_status_no_disk(self):
        """DISK? returns false when no storage image."""
        text = self._run_kdos(["DISK? ."])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("0 ", out)

    def test_disk_status_with_disk(self):
        """DISK? returns true when storage image attached."""
        with tempfile.NamedTemporaryFile(suffix=".img", delete=False) as f:
            path = f.name
        try:
            os.unlink(path)
            text = self._run_kdos(["DISK? ."], storage_image=path)
            self.assertIn("-1 ", text)  # true = -1
        finally:
            if os.path.exists(path):
                os.unlink(path)

    def test_disk_info(self):
        """DISK-INFO prints storage status."""
        text = self._run_kdos(["DISK-INFO"])
        self.assertIn("Storage:", text)
        self.assertIn("not attached", text)

    def test_disk_info_present(self):
        """DISK-INFO shows present when image attached."""
        with tempfile.NamedTemporaryFile(suffix=".img", delete=False) as f:
            path = f.name
        try:
            os.unlink(path)
            text = self._run_kdos(["DISK-INFO"], storage_image=path)
            self.assertIn("Storage:", text)
            self.assertIn("present", text)
        finally:
            if os.path.exists(path):
                os.unlink(path)

    def test_bsave_bload_roundtrip(self):
        """B.SAVE writes buffer to disk, B.LOAD reads it back."""
        with tempfile.NamedTemporaryFile(suffix=".img", delete=False) as f:
            path = f.name
        try:
            os.unlink(path)
            text = self._run_kdos([
                "0 1 64 BUFFER sb",
                "42 sb B.FILL",
                "sb 0 B.SAVE",
                "sb B.ZERO",
                "sb B.DATA C@ .",
                "sb 0 B.LOAD",
                "sb B.DATA C@ .",
            ], storage_image=path)
            idx = text.rfind("HELP")
            out = text[idx:] if idx >= 0 else text
            # After zero, first byte = 0
            self.assertIn("0 ", out)
            # After load, first byte = 42
            self.assertIn("42 ", out)
        finally:
            if os.path.exists(path):
                os.unlink(path)

    def test_bsectors(self):
        """B.SECTORS returns correct sector count."""
        text = self._run_kdos([
            "0 1 64 BUFFER s1",
            "s1 B.SECTORS .",
            "0 1 1024 BUFFER s2",
            "s2 B.SECTORS .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("1 ", out)  # 64 bytes -> 1 sector
        self.assertIn("2 ", out)  # 1024 bytes -> 2 sectors

    def test_file_create(self):
        """FILE creates a file descriptor."""
        text = self._run_kdos([
            "10 4 FILE myfile",
            "myfile F.START .",
            "myfile F.MAX .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("10 ", out)  # start sector
        self.assertIn("4 ", out)   # max sectors

    def test_file_info(self):
        """F.INFO prints file descriptor."""
        text = self._run_kdos([
            "0 2 FILE fi",
            "fi F.INFO",
        ])
        self.assertIn("[file", text)
        self.assertIn("sec=", text)
        self.assertIn("max=", text)
        self.assertIn("used=", text)

    def test_file_seek_rewind(self):
        """FSEEK sets cursor, FREWIND resets it."""
        text = self._run_kdos([
            "0 2 FILE sf",
            "100 sf FSEEK",
            "sf F.CURSOR .",
            "sf FREWIND",
            "sf F.CURSOR .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("100 ", out)
        # After rewind cursor should be 0
        after_100 = out[out.find("100 ") + 4:]
        self.assertIn("0 ", after_100)

    def test_files_list(self):
        """FILES lists registered files."""
        text = self._run_kdos([
            "0 2 FILE f1",
            "10 4 FILE f2",
            "FILES",
        ])
        self.assertIn("Files", text)
        self.assertIn("[file", text)

    def test_file_count(self):
        """FILE-COUNT tracks registered files."""
        text = self._run_kdos([
            "0 1 FILE fc1",
            "1 1 FILE fc2",
            "FILE-COUNT @ .",
        ])
        idx = text.rfind("HELP")
        out = text[idx:] if idx >= 0 else text
        self.assertIn("2 ", out)


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
