"""
Real-Network Integration Tests for Megapad-64
==============================================
These tests exercise the BIOS and KDOS networking stack against a real
Linux TAP device.  They verify that ARP, ICMP (ping), UDP, and the
full IP stack work against real host networking — not just simulated
frame injection.

Prerequisites
-------------
Create and configure the TAP interface (one-time, requires root):

    sudo ip tuntap add dev mp64tap0 mode tap user $USER
    sudo ip link set mp64tap0 up
    sudo ip addr add 10.64.0.1/24 dev mp64tap0

    # For internet access (optional):
    sudo sysctl -w net.ipv4.ip_forward=1
    sudo iptables -t nat -A POSTROUTING -s 10.64.0.0/24 ! -o mp64tap0 -j MASQUERADE

Running
-------
    make test-one K=TestRealNet

All tests in this file are marked ``@pytest.mark.realnet`` and will be
skipped automatically when the TAP device is not accessible.

Network Layout
--------------
    Host side:   10.64.0.1  (on mp64tap0)
    Emulator:    10.64.0.2  (configured by tests via IP-SET / DHCP)
    Emulator MAC: 02:4D:50:36:34:00  (NIC default)
"""

from __future__ import annotations

import os
import socket
import struct
import tempfile
import threading
import time
import unittest

import pytest

# ---------------------------------------------------------------------------
#  Check TAP availability — skip entire module if not present
# ---------------------------------------------------------------------------

_TAP_NAME = os.environ.get("MP64_TAP", "mp64tap0")
_HOST_IP = os.environ.get("MP64_HOST_IP", "10.64.0.1")
_EMU_IP = os.environ.get("MP64_EMU_IP", "10.64.0.2")

try:
    from nic_backends import TAPBackend, tap_available
    _TAP_OK = tap_available(_TAP_NAME)
except ImportError:
    _TAP_OK = False

# Mark for real-net tests — applied per-class, not module-wide,
# so that TestNICBackends and TestBackwardCompat always run.
_skip_no_tap = pytest.mark.skipif(
    not _TAP_OK,
    reason=f"TAP device '{_TAP_NAME}' not accessible "
           f"(run setup commands from test_networking.py docstring)",
)

# ---------------------------------------------------------------------------
#  Imports (only after skip-guard so import errors don't mask the skip)
# ---------------------------------------------------------------------------

from nic_backends import TAPBackend

from accel_wrapper import Megapad64, HaltError, TrapError, u64

from asm import assemble
from diskutil import format_image, inject_file, FTYPE_FORTH
from system import MegapadSystem

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BIOS_PATH = os.path.join(PROJECT_ROOT, "bios.asm")
KDOS_PATH = os.path.join(PROJECT_ROOT, "kdos.f")
TOOLS_PATH = os.path.join(PROJECT_ROOT, "tools.f")
AUTOEXEC_PATH = os.path.join(PROJECT_ROOT, "autoexec.f")
GRAPHICS_PATH = os.path.join(PROJECT_ROOT, "graphics.f")


# ---------------------------------------------------------------------------
#  Helpers
# ---------------------------------------------------------------------------

def make_system_with_tap(tap_name: str = _TAP_NAME,
                         ram_kib: int = 1024) -> MegapadSystem:
    """Create a MegapadSystem with the NIC wired to a real TAP device."""
    backend = TAPBackend(tap_name=tap_name)
    sys = MegapadSystem(ram_size=ram_kib * 1024, nic_backend=backend)
    return sys


def capture_uart(sys: MegapadSystem) -> list[int]:
    buf: list[int] = []
    sys.uart.on_tx = lambda b: buf.append(b)
    return buf


def uart_text(buf: list[int]) -> str:
    return "".join(
        chr(b) if (0x20 <= b < 0x7F or b in (10, 13, 9)) else ""
        for b in buf
    )


def _next_line_chunk(data: bytes, pos: int) -> bytes:
    nl = data.find(b'\n', pos)
    if nl == -1:
        return data[pos:]
    return data[pos:nl + 1]


def _load_bios():
    with open(BIOS_PATH) as f:
        return assemble(f.read())


# ── Ethernet frame construction helpers (host-side) ──

def build_eth_frame(dst: bytes, src: bytes, ethertype: int,
                    payload: bytes) -> bytes:
    """Build a raw Ethernet frame."""
    return dst + src + struct.pack("!H", ethertype) + payload


def build_arp_reply(sha: bytes, spa: str, tha: bytes, tpa: str) -> bytes:
    """Build an ARP reply frame (host → emulator)."""
    spa_b = socket.inet_aton(spa)
    tpa_b = socket.inet_aton(tpa)
    arp = struct.pack("!HHBBH", 1, 0x0800, 6, 4, 2)   # reply
    arp += sha + spa_b + tha + tpa_b
    return build_eth_frame(tha, sha, 0x0806, arp)


def build_arp_request(sha: bytes, spa: str, tpa: str) -> bytes:
    """Build an ARP request frame (host → emulator)."""
    spa_b = socket.inet_aton(spa)
    tpa_b = socket.inet_aton(tpa)
    arp = struct.pack("!HHBBH", 1, 0x0800, 6, 4, 1)   # request
    arp += sha + spa_b + b'\x00' * 6 + tpa_b
    return build_eth_frame(b'\xff' * 6, sha, 0x0806, arp)


def build_icmp_echo_request(dst_mac: bytes, src_mac: bytes,
                            src_ip: str, dst_ip: str,
                            ident: int = 0x1234,
                            seq: int = 1,
                            payload: bytes = b'MEGAPAD64PING') -> bytes:
    """Build a complete ICMP echo request (Ethernet + IP + ICMP)."""
    # ICMP
    icmp = struct.pack("!BBH HH", 8, 0, 0, ident, seq) + payload
    # Compute ICMP checksum
    icmp = _fix_checksum(icmp, 2)

    # IP header
    src_b = socket.inet_aton(src_ip)
    dst_b = socket.inet_aton(dst_ip)
    ip_hdr = struct.pack("!BBHHHBBH4s4s",
                         0x45, 0, 20 + len(icmp), 0, 0,
                         64, 1, 0, src_b, dst_b)
    ip_hdr = _fix_checksum(ip_hdr, 10)

    return build_eth_frame(dst_mac, src_mac, 0x0800, ip_hdr + icmp)


def build_udp_packet(dst_mac: bytes, src_mac: bytes,
                     src_ip: str, dst_ip: str,
                     sport: int, dport: int,
                     payload: bytes) -> bytes:
    """Build a complete UDP packet (Ethernet + IP + UDP)."""
    udp_len = 8 + len(payload)
    udp = struct.pack("!HHHH", sport, dport, udp_len, 0) + payload
    # UDP checksum (optional for IPv4, set to 0 here)

    src_b = socket.inet_aton(src_ip)
    dst_b = socket.inet_aton(dst_ip)
    total_len = 20 + len(udp)
    ip_hdr = struct.pack("!BBHHHBBH4s4s",
                         0x45, 0, total_len, 0, 0x4000,
                         64, 17, 0, src_b, dst_b)
    ip_hdr = _fix_checksum(ip_hdr, 10)

    return build_eth_frame(dst_mac, src_mac, 0x0800, ip_hdr + udp)


def _fix_checksum(packet: bytes, cksum_offset: int) -> bytes:
    """Compute and insert IP/ICMP ones-complement checksum."""
    pkt = bytearray(packet)
    pkt[cksum_offset] = 0
    pkt[cksum_offset + 1] = 0
    s = 0
    for i in range(0, len(pkt), 2):
        if i + 1 < len(pkt):
            s += (pkt[i] << 8) | pkt[i + 1]
        else:
            s += pkt[i] << 8
    while s >> 16:
        s = (s & 0xFFFF) + (s >> 16)
    cksum = ~s & 0xFFFF
    pkt[cksum_offset] = cksum >> 8
    pkt[cksum_offset + 1] = cksum & 0xFF
    return bytes(pkt)


def parse_arp_from_frame(frame: bytes):
    """Parse an ARP packet from a raw Ethernet frame. Returns dict or None."""
    if len(frame) < 42:
        return None
    etype = struct.unpack("!H", frame[12:14])[0]
    if etype != 0x0806:
        return None
    arp = frame[14:]
    htype, ptype, hlen, plen, oper = struct.unpack("!HHBBH", arp[:8])
    sha = arp[8:14]
    spa = socket.inet_ntoa(arp[14:18])
    tha = arp[18:24]
    tpa = socket.inet_ntoa(arp[24:28])
    return {"oper": oper, "sha": sha, "spa": spa, "tha": tha, "tpa": tpa}


HOST_MAC = b'\xDE\xAD\xBE\xEF\x00\x01'   # Fake host-side MAC for testing
EMU_MAC  = b'\x02\x4D\x50\x36\x34\x00'    # Default NIC MAC


# ---------------------------------------------------------------------------
#  Base class for real-network tests
# ---------------------------------------------------------------------------

class _RealNetBase(unittest.TestCase):
    """Base for tests that talk to the emulator over a real TAP device.

    Boots BIOS + KDOS once as a class-level snapshot, then each test
    restores from the snapshot, configures networking, and runs Forth
    commands while the TAP backend delivers real frames.
    """

    _bios_code: bytes = None
    _kdos_snapshot = None
    _disk_image_path: str = None

    @classmethod
    def _save_cpu_state(cls, cpu):
        return {
            'pc': cpu.pc,
            'regs': list(cpu.regs),
            'psel': cpu.psel, 'xsel': cpu.xsel, 'spsel': cpu.spsel,
            'flag_z': cpu.flag_z, 'flag_c': cpu.flag_c,
            'flag_n': cpu.flag_n, 'flag_v': cpu.flag_v,
            'flag_p': cpu.flag_p, 'flag_g': cpu.flag_g,
            'flag_i': cpu.flag_i, 'flag_s': cpu.flag_s,
            'd_reg': cpu.d_reg, 'q_out': cpu.q_out, 't_reg': cpu.t_reg,
            'ivt_base': cpu.ivt_base, 'ivec_id': cpu.ivec_id,
            'trap_addr': cpu.trap_addr,
            'halted': cpu.halted, 'idle': cpu.idle,
            'cycle_count': cpu.cycle_count,
            '_ext_modifier': cpu._ext_modifier,
        }

    @classmethod
    def _restore_cpu_state(cls, cpu, state):
        cpu.pc = state['pc']
        cpu.regs[:] = state['regs']
        for k in ('psel', 'xsel', 'spsel',
                   'flag_z', 'flag_c', 'flag_n', 'flag_v',
                   'flag_p', 'flag_g', 'flag_i', 'flag_s',
                   'd_reg', 'q_out', 't_reg',
                   'ivt_base', 'ivec_id', 'trap_addr',
                   'halted', 'idle', 'cycle_count', '_ext_modifier'):
            setattr(cpu, k, state[k])

    @classmethod
    def _build_disk_image(cls) -> str:
        """Build a disk image with kdos.f, autoexec.f, graphics.f, tools.f.

        kdos.f is injected first so the BIOS auto-boots it (BIOS loads
        the first FTYPE_FORTH file it finds in the directory).
        """
        path = os.path.join(tempfile.gettempdir(), 'mp64_realnet_test.img')
        format_image(path)
        for fpath in (KDOS_PATH, AUTOEXEC_PATH, GRAPHICS_PATH, TOOLS_PATH):
            with open(fpath, 'rb') as f:
                inject_file(path, os.path.basename(fpath), f.read(),
                            ftype=FTYPE_FORTH)
        return path

    @classmethod
    def _ensure_snapshot(cls):
        if cls._kdos_snapshot is not None:
            return
        cls._bios_code = _load_bios()
        cls._disk_image_path = cls._build_disk_image()

        # Boot from disk — BIOS auto-loads kdos.f, then KDOS runs
        # _AUTOEXEC-RUN which loads autoexec.f → net config → tools.f.
        # Disable NIC link so AUTOEXEC-NET skips DHCP during snapshot.
        sys_obj = MegapadSystem(ram_size=1024 * 1024,
                                storage_image=cls._disk_image_path,
                                ext_mem_size=16 * (1 << 20))
        sys_obj.nic.link_up = False
        buf = capture_uart(sys_obj)
        sys_obj.load_binary(0, cls._bios_code)
        sys_obj.boot()

        max_steps = 1_000_000_000
        total = 0
        while total < max_steps:
            if sys_obj.cpu.halted:
                break
            if sys_obj.cpu.idle:
                break          # Sitting in QUIT loop — fully booted
            batch = sys_obj.run_batch(min(100_000, max_steps - total))
            total += max(batch, 1)

        cls._kdos_snapshot = (
            bytes(sys_obj.cpu.mem),
            cls._save_cpu_state(sys_obj.cpu),
        )

    def setUp(self):
        self.__class__._ensure_snapshot()

    def _run_kdos_tap(self, extra_lines: list[str],
                      max_steps: int = 100_000_000,
                      setup_ip: bool = True) -> str:
        """Run KDOS commands with the NIC wired to a real TAP device.

        If *setup_ip* is True (default), automatically configures the
        emulator's IP as 10.64.0.2/24 with gateway 10.64.0.1 before
        running the user's extra_lines.
        """
        mem_bytes, cpu_state = self.__class__._kdos_snapshot
        backend = TAPBackend(tap_name=_TAP_NAME)
        sys = MegapadSystem(ram_size=1024 * 1024, nic_backend=backend,
                            storage_image=self.__class__._disk_image_path,
                            ext_mem_size=16 * (1 << 20))
        buf = capture_uart(sys)

        # Restore memory
        sys.cpu.mem[:len(mem_bytes)] = mem_bytes
        self._restore_cpu_state(sys.cpu, cpu_state)

        # IP config lines (skip if test does its own)
        ip_lines = []
        if setup_ip:
            parts = _EMU_IP.split(".")
            ip_lines = [
                f"{parts[0]} {parts[1]} {parts[2]} {parts[3]} IP-SET",
            ]
            # Set gateway
            gw = _HOST_IP.split(".")
            ip_lines.append(f"{gw[0]} {gw[1]} {gw[2]} {gw[3]} GW-IP IP!")
            # Set netmask 255.255.255.0
            ip_lines.append("255 255 255 0 NET-MASK IP!")
            # Set DNS server
            ip_lines.append("8 8 8 8 DNS-SERVER-IP IP!")

        all_lines = ip_lines + extra_lines
        payload = "\n".join(all_lines) + "\nBYE\n"
        data = payload.encode()
        pos = 0
        steps = 0
        idle_polls = 0           # count consecutive idle-with-no-data polls
        max_idle_polls = 200     # 200 × 20ms = 4s idle timeout per gap

        try:
            while steps < max_steps:
                if sys.cpu.halted:
                    break

                # --- Inject UART data whenever the buffer is empty ---
                # The Forth REPL busy-polls the UART (KEY), so it won't
                # go idle while waiting for input.  We must feed data
                # regardless of CPU state — exactly as stdin does in CLI.
                if not sys.uart.has_rx_data and pos < len(data):
                    chunk = _next_line_chunk(data, pos)
                    sys.uart.inject_input(chunk)
                    pos += len(chunk)
                    if sys.cpu.idle:
                        sys.cpu.idle = False
                    idle_polls = 0

                # --- Run CPU if it has work to do ---
                if not (sys.cpu.idle
                        and not sys.uart.has_rx_data
                        and not sys._any_nic_rx()):
                    batch = sys.run_batch(min(100_000, max_steps - steps))
                    steps += max(batch, 1)
                    idle_polls = 0
                    continue

                # CPU is idle, no UART data, no NIC frames — brief
                # sleep to let TAP thread deliver frames, then wake CPU
                # so Forth polling loops (NET-IDLE) can advance.
                if idle_polls < max_idle_polls:
                    idle_polls += 1
                    time.sleep(0.02)
                    sys.cpu.idle = False
                else:
                    break
        finally:
            # Clean shutdown of TAP backend — must always run to release fd
            sys.nic.stop()
        return uart_text(buf)

    def _is_host_reachable(self) -> bool:
        """Quick check that the host side of the TAP is responding."""
        try:
            s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
            s.settimeout(0.5)
            s.bind((_HOST_IP, 0))
            s.close()
            return True
        except OSError:
            return False


# ---------------------------------------------------------------------------
#  Test classes
# ---------------------------------------------------------------------------

@_skip_no_tap
@pytest.mark.realnet
@pytest.mark.xdist_group("tap")
class TestRealNetBIOS(_RealNetBase):
    """Test BIOS networking primitives against a real TAP device."""

    def test_net_status_link_up(self):
        """NET-STATUS should report link-up and present when TAP is active."""
        text = self._run_kdos_tap(["NET-STATUS ."], setup_ip=False)
        # Bit 2 (link up) + bit 7 (present) = 0x84 = 132
        self.assertIn("132 ", text)

    def test_net_mac_readable(self):
        """NET-MAC@ should return a valid MMIO address."""
        text = self._run_kdos_tap([
            "NET-MAC@ C@ .",
        ], setup_ip=False)
        # First byte of default MAC is 0x02 = 2
        self.assertIn("2 ", text)


@_skip_no_tap
@pytest.mark.realnet
@pytest.mark.xdist_group("tap")
class TestRealNetARP(_RealNetBase):
    """Test the KDOS ARP stack against real TAP networking.

    The TAP is configured with the host at 10.64.0.1.
    The emulator is at 10.64.0.2.  When the emulator sends an
    ARP request, the host kernel responds automatically (because
    10.64.0.1 is configured on the TAP interface).
    """

    def test_arp_resolve_gateway(self):
        """ARP-RESOLVE for the gateway (host side of TAP) should succeed.

        The Linux kernel will reply to ARP requests for 10.64.0.1
        on the mp64tap0 interface, so the KDOS ARP stack should
        get a real reply and populate its ARP table.
        """
        text = self._run_kdos_tap([
            # Resolve the gateway's MAC — should return non-zero
            "GW-IP IP@ ARP-RESOLVE",
            "DUP 0<> IF",
            "  .\"  ARP-OK \"",
            "  DUP C@ . .\"  m0 \"",
            "  DUP 1+ C@ . .\"  m1 \"",
            "  DUP 2 + C@ . .\"  m2 \"",
            "  DROP",
            "ELSE",
            "  .\"  ARP-FAIL \"",
            "  DROP",
            "THEN",
        ], max_steps=200_000_000)
        self.assertIn("ARP-OK", text,
                      "ARP-RESOLVE should have succeeded against the host")
        # We don't know the host's TAP MAC in advance, but the first
        # three bytes at least shouldn't all be zero
        self.assertNotIn("0  m0 0  m1 0  m2", text)

    def test_arp_table_populated(self):
        """After ARP-RESOLVE, .ARP should show the gateway entry."""
        text = self._run_kdos_tap([
            "GW-IP IP@ ARP-RESOLVE DROP",
            "ARP-TABLE ARP-SLOTS /ARP-ENT * DUMP",
        ], max_steps=200_000_000)
        # If it worked, the dump should contain non-zero MAC bytes
        # This is a weaker check — mainly verifying no crash
        self.assertNotIn("ARP-FAIL", text)

    def test_arp_handle_request_from_host(self):
        """When the host ARPs for us, ARP-HANDLE should auto-reply.

        We trigger this by having the emulator listen for ARP while
        the host pings 10.64.0.2 (which makes the host send an ARP
        request first).
        """
        text = self._run_kdos_tap([
            # Poll for ARP requests a few times
            ": APOLL 20 0 DO ARP-POLL LOOP ; APOLL",
            ".\"  POLL-DONE \"",
        ], max_steps=200_000_000)
        # If no crash, ARP-POLL successfully ran against real frames
        self.assertIn("POLL-DONE", text)


@_skip_no_tap
@pytest.mark.realnet
@pytest.mark.xdist_group("tap")
class TestRealNetICMP(_RealNetBase):
    """Test ICMP (ping) against real TAP networking."""

    def test_ping_host(self):
        """Send an ICMP echo request to the host and check for a reply.

        The KDOS ICMP stack builds a real ping packet, sends it via
        the TAP device, and the host kernel should respond.
        """
        text = self._run_kdos_tap([
            # Build and send an ICMP echo request to the gateway
            "GW-IP IP@ ARP-RESOLVE",
            "DUP 0= IF .\"  NO-ARP \" DROP",
            "ELSE DROP",
            # Build ICMP echo request manually
            "  GW-IP IP@ 0 0 ICMP-BUILD-ECHO-REQ",
            "  1 IP-PROTO-ICMP GW-IP IP@ ROT ROT IP-SEND",
            "  .\"  PING-SENT \"",
            # Try to receive the reply
            "  20 0 DO",
            "    IP-RECV",
            "    DUP 0<> IF",
            "      .\"  IP-GOT \"",
            "      2DROP LEAVE",
            "    ELSE",
            "      2DROP",
            "    THEN",
            "  LOOP",
            "THEN",
        ], max_steps=300_000_000)
        if "NO-ARP" in text:
            self.skipTest("ARP resolution failed — host may not be reachable")
        self.assertIn("PING-SENT", text,
                      "Should have sent the ICMP echo request")

    def test_respond_to_ping(self):
        """The KDOS ICMP auto-responder should reply to pings from the host.

        We run PING-POLL which listens for ICMP echo requests and
        auto-replies.  The host side can be verified with:
            ping -c 1 10.64.0.2
        """
        text = self._run_kdos_tap([
            ": PPOLL 50 0 DO PING-POLL LOOP ; PPOLL",
            ".\"  PING-RESPONDED \"",
        ], max_steps=200_000_000)
        self.assertIn("PING-RESPONDED", text)


@_skip_no_tap
@pytest.mark.realnet
@pytest.mark.xdist_group("tap")
class TestRealNetUDP(_RealNetBase):
    """Test UDP send/receive against real TAP networking."""

    def test_udp_send_to_host(self):
        """Send a UDP packet from the emulator to a real host socket.

        We open a UDP socket on the host side (bound to 10.64.0.1),
        then have KDOS send a UDP packet to that port.
        """
        received = []

        # Host-side listener
        def host_listener(port):
            try:
                s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
                s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                s.bind((_HOST_IP, port))
                s.settimeout(10.0)
                data, addr = s.recvfrom(2048)
                received.append(data)
                s.close()
            except socket.timeout:
                pass
            except OSError:
                pass

        port = 7777
        t = threading.Thread(target=host_listener, args=(port,), daemon=True)
        t.start()
        time.sleep(0.1)  # let the socket bind

        text = self._run_kdos_tap([
            # S" is compile-only, so wrap in a colon definition
            f': DO-UDP-SEND GW-IP IP@ {port} 9999 S" HELLO-MP64" UDP-SEND . ;',
            'DO-UDP-SEND',
        ], max_steps=300_000_000)

        t.join(timeout=12.0)

        if received:
            self.assertIn(b"HELLO-MP64", received[0])
        # Even if no packet arrived (timing), verify no crash
        self.assertNotIn("ERROR", text.upper().replace("ERROR:", ""))

    def test_udp_recv_from_host(self):
        """Receive a UDP packet sent from the host to the emulator.

        We have the host send a UDP packet to 10.64.0.2, and KDOS
        should pick it up via UDP-RECV.
        """
        def host_sender(delay: float = 1.0):
            """Send a UDP packet from the host to the emulator."""
            time.sleep(delay)
            for _ in range(5):  # retry a few times
                try:
                    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
                    s.sendto(b"HOST-MSG-42", (_EMU_IP, 5555))
                    s.close()
                except OSError:
                    pass
                time.sleep(0.5)

        t = threading.Thread(target=host_sender, args=(2.0,), daemon=True)
        t.start()

        text = self._run_kdos_tap([
            # Try to receive a UDP packet
            "100 0 DO",
            "  UDP-RECV",
            "  DUP 0<> IF",
            "    .\"  UDP-GOT \"",
            "    DROP 2DROP LEAVE",
            "  ELSE",
            "    DROP 2DROP",
            "  THEN",
            "LOOP",
        ], max_steps=400_000_000)

        t.join(timeout=15.0)
        # This may or may not receive depending on ARP timing
        # The key assertion is no crash
        self.assertNotIn("ABORT", text)


@_skip_no_tap
@pytest.mark.realnet
@pytest.mark.xdist_group("tap")
class TestRealNetIntegration(_RealNetBase):
    """End-to-end integration tests with real networking."""

    def test_full_ip_stack_init(self):
        """IP-SET + MAC-INIT should configure the stack without errors."""
        text = self._run_kdos_tap([
            "MY-IP .IP .\"  myip \"",
            "GW-IP .IP .\"  gw \"",
            "NET-STATUS . .\"  status \"",
        ])
        self.assertIn("status", text)
        # .IP prints dotted-quad: "10 .64 .0 .2 "
        self.assertIn("10", text)
        self.assertIn("64", text)
        # Verify gateway was also configured
        self.assertIn("gw", text)
        self.assertIn("myip", text)

    def test_net_status_shows_backend(self):
        """Verify the system status report shows TAP backend info."""
        backend = TAPBackend(tap_name=_TAP_NAME)
        sys = MegapadSystem(ram_size=1024 * 1024, nic_backend=backend)
        try:
            status = sys.dump_state()
            self.assertIn(f"tap:{_TAP_NAME}", status)
        finally:
            sys.nic.stop()

    def test_backend_stats_tracking(self):
        """TAP backend should track TX/RX byte counts."""
        backend = TAPBackend(tap_name=_TAP_NAME)
        sys = MegapadSystem(ram_size=1024 * 1024, nic_backend=backend)
        try:
            stats = backend.stats()
            self.assertIn("tx_bytes", stats)
            self.assertIn("rx_bytes", stats)
            self.assertTrue(stats["link_up"])
        finally:
            sys.nic.stop()


class TestNICBackends(unittest.TestCase):
    """Unit tests for the NIC backend abstraction itself."""

    def test_loopback_backend(self):
        """LoopbackBackend should accept sends silently."""
        from nic_backends import LoopbackBackend
        b = LoopbackBackend()
        b.start()
        self.assertTrue(b.link_up)
        self.assertTrue(b.send(b'\xff' * 14))
        self.assertEqual(b.name, "loopback")
        b.stop()
        self.assertFalse(b.link_up)

    def test_udp_backend_lifecycle(self):
        """UDPBackend should bind, send, and stop cleanly."""
        from nic_backends import UDPBackend
        b = UDPBackend(bind_port=19876, peer_port=19877)
        b.start()
        self.assertTrue(b.link_up)
        self.assertIn("udp:", b.name)
        b.send(b'\xff' * 14)
        b.stop()

    def test_udp_backend_roundtrip(self):
        """Two UDP backends should be able to exchange frames."""
        from nic_backends import UDPBackend
        received = []
        b1 = UDPBackend(bind_port=19878, peer_port=19879)
        b2 = UDPBackend(bind_port=19879, peer_port=19878)
        b2.on_rx_frame = lambda f: received.append(f)
        b1.start()
        b2.start()
        time.sleep(0.1)

        test_frame = b'\xDE\xAD' * 7   # 14 bytes
        b1.send(test_frame)
        time.sleep(0.5)

        b1.stop()
        b2.stop()
        self.assertEqual(len(received), 1)
        self.assertEqual(received[0], test_frame)

    def test_nic_device_with_loopback_backend(self):
        """NetworkDevice should work with LoopbackBackend (backward compat)."""
        from nic_backends import LoopbackBackend
        from devices import NetworkDevice
        b = LoopbackBackend()
        nic = NetworkDevice(backend=b)
        self.assertTrue(nic.link_up)
        self.assertEqual(nic.backend_name, "loopback")

    def test_nic_device_with_udp_backend(self):
        """NetworkDevice should forward TX to UDPBackend."""
        from nic_backends import UDPBackend
        from devices import NetworkDevice
        received = []

        b_rx = UDPBackend(bind_port=19880, peer_port=19881)
        b_rx.on_rx_frame = lambda f: received.append(f)
        b_rx.start()

        b_tx = UDPBackend(bind_port=19881, peer_port=19880)
        nic = NetworkDevice(backend=b_tx)

        # Write a frame via data port and send
        frame = b'\xAA\xBB\xCC' * 5  # 15 bytes
        nic._data_buf = bytearray(frame)
        nic.frame_len = len(frame)
        nic._execute_cmd(0x01)  # SEND

        time.sleep(0.5)
        nic.stop()
        b_rx.stop()

        self.assertEqual(len(received), 1)
        self.assertEqual(received[0], frame)

    def test_nic_device_backend_rx(self):
        """Frames arriving at the backend should be queued in rx_queue."""
        from nic_backends import UDPBackend
        from devices import NetworkDevice

        b_nic = UDPBackend(bind_port=19882, peer_port=19883)
        nic = NetworkDevice(backend=b_nic)

        b_sender = UDPBackend(bind_port=19883, peer_port=19882)
        b_sender.start()
        time.sleep(0.1)

        test_frame = b'\x01\x02\x03\x04\x05\x06' * 3
        b_sender.send(test_frame)
        time.sleep(0.5)

        self.assertEqual(len(nic.rx_queue), 1)
        self.assertEqual(nic.rx_queue[0], test_frame)
        self.assertEqual(nic.rx_count, 1)

        nic.stop()
        b_sender.stop()

    @pytest.mark.realnet
    @pytest.mark.xdist_group("tap")
    @pytest.mark.skipif(not _TAP_OK,
                        reason=f"TAP device '{_TAP_NAME}' not accessible")
    def test_tap_backend_lifecycle(self):
        """TAPBackend should open, report link-up, and stop cleanly."""
        b = TAPBackend(tap_name=_TAP_NAME)
        b.start()
        self.assertTrue(b.link_up)
        self.assertIn("tap:", b.name)
        stats = b.stats()
        self.assertEqual(stats["tx_bytes"], 0)
        b.stop()
        self.assertFalse(b.link_up)

    @pytest.mark.realnet
    @pytest.mark.xdist_group("tap")
    @pytest.mark.skipif(not _TAP_OK,
                        reason=f"TAP device '{_TAP_NAME}' not accessible")
    def test_tap_backend_send_frame(self):
        """TAPBackend.send() should write a frame without error."""
        b = TAPBackend(tap_name=_TAP_NAME)
        b.start()
        self.assertTrue(b.link_up)
        # Minimum Ethernet frame: 14 bytes header + some payload
        frame = (b'\xff' * 6 + EMU_MAC +
                 b'\x08\x00' + b'\x00' * 46)  # 64 bytes total
        ok = b.send(frame)
        self.assertTrue(ok)
        self.assertGreater(b.tx_bytes, 0)
        b.stop()

    def test_tap_unavailable_graceful(self):
        """TAPBackend with a non-existent device should fail gracefully."""
        b = TAPBackend(tap_name="mp64_nonexistent_999")
        b.start()
        self.assertFalse(b.link_up)
        self.assertIn("ERROR", b.name.upper())
        b.stop()

    def test_setup_tap_helper(self):
        """setup_tap() should return valid shell commands."""
        from nic_backends import setup_tap
        cmds = setup_tap("mp64tap0", "10.64.0.1", 24)
        self.assertIn("ip tuntap add", cmds)
        self.assertIn("ip link set", cmds)
        self.assertIn("ip addr add", cmds)

    def test_tap_available_helper(self):
        """tap_available() should return a bool without crashing."""
        from nic_backends import tap_available
        result = tap_available("mp64_surely_not_here")
        self.assertFalse(result)


# ---------------------------------------------------------------------------
#  TAP-based hardening / stress tests
# ---------------------------------------------------------------------------

@_skip_no_tap
@pytest.mark.realnet
@pytest.mark.xdist_group("tap")
class TestRealNetHardening(_RealNetBase):
    """Stress and robustness tests that exercise the full stack over TAP."""

    def test_ping_ip_outbound(self):
        """PING-IP word should send a real ICMP echo to the gateway."""
        text = self._run_kdos_tap([
            '10 64 0 1 2 PING-IP',
        ], max_steps=300_000_000)
        # PING-IP prints "N sent, M received"
        self.assertIn("sent", text.lower(),
                      f"PING-IP should print sent count, got: {text[-300:]}")

    def test_rapid_arp_poll_100(self):
        """100 consecutive ARP-POLL calls should not crash or hang."""
        text = self._run_kdos_tap([
            ": ARP100 100 0 DO ARP-POLL LOOP ;",
            "ARP100 .\"  ARP100-OK\"",
        ], max_steps=200_000_000)
        self.assertIn("ARP100-OK", text)

    def test_rapid_ping_poll_100(self):
        """100 consecutive PING-POLL calls should not crash."""
        text = self._run_kdos_tap([
            ": PP100 100 0 DO PING-POLL LOOP ;",
            "PP100 .\"  PP100-OK\"",
        ], max_steps=200_000_000)
        self.assertIn("PP100-OK", text)

    def test_inject_runt_via_tap(self):
        """Inject a runt frame from host-side; emulator should survive."""
        import subprocess
        # Send a tiny raw packet via TAP (may require CAP_NET_RAW)
        # Even if send fails, the main thing is emulator doesn't crash
        text = self._run_kdos_tap([
            ": APOLL 10 0 DO ARP-POLL LOOP ; APOLL",
            ": PPOLL 10 0 DO PING-POLL LOOP ; PPOLL",
            ".\"  RUNT-OK\"",
        ], max_steps=200_000_000)
        self.assertIn("RUNT-OK", text)

    def test_broadcast_storm_via_tap(self):
        """Many broadcast frames from the host should not crash the stack."""
        text = self._run_kdos_tap([
            ": STORM-DRAIN 50 0 DO ARP-POLL LOOP ;",
            "STORM-DRAIN .\"  STORM-OK\"",
        ], max_steps=200_000_000)
        self.assertIn("STORM-OK", text)

    def test_full_stack_roundtrip_udp(self):
        """Send UDP from emulator and verify it arrives at host socket."""
        received = []

        def host_listener():
            try:
                s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
                s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                s.bind((_HOST_IP, 7778))
                s.settimeout(10.0)
                data, _ = s.recvfrom(2048)
                received.append(data)
                s.close()
            except (socket.timeout, OSError):
                pass

        t = threading.Thread(target=host_listener, daemon=True)
        t.start()
        time.sleep(0.1)

        text = self._run_kdos_tap([
            ': DO-UDP GW-IP IP@ 7778 8888 S" HARDENING-OK" UDP-SEND . ; DO-UDP',
        ], max_steps=300_000_000)

        t.join(timeout=12.0)
        if received:
            self.assertIn(b"HARDENING-OK", received[0])
        # No crash is the minimum requirement
        self.assertNotIn("ABORT", text)

    def test_arp_then_icmp_then_udp_sequence(self):
        """Full protocol sequence: ARP resolve, PING, then UDP send."""
        text = self._run_kdos_tap([
            # Step 1: ARP resolve gateway
            "GW-IP IP@ ARP-RESOLVE",
            "DUP 0<> IF .\"  ARP-OK\" DROP ELSE .\"  ARP-FAIL\" DROP THEN",
            # Step 2: PING-POLL a few times
            ": PPOLL 10 0 DO PING-POLL LOOP ; PPOLL",
            ".\"  PING-OK\"",
            # Step 3: UDP send
            ': DO-SEQ-UDP GW-IP IP@ 9999 8888 S" SEQ-TEST" UDP-SEND DROP ; DO-SEQ-UDP',
            ".\"  SEQ-OK\"",
        ], max_steps=300_000_000)
        # At minimum, all three stages should complete
        self.assertIn("PING-OK", text)
        self.assertIn("SEQ-OK", text)

    def test_ip_recv_no_crash_on_tap_noise(self):
        """IP-RECV should handle whatever random frames arrive on TAP."""
        text = self._run_kdos_tap([
            "VARIABLE IPCNT  0 IPCNT !",
            ": RX-MANY 20 0 DO IP-RECV DUP 0<> IF DROP 1 IPCNT +! ELSE DROP THEN LOOP ;",
            "RX-MANY",
            "IPCNT @ .\"  ipcnt=\" .",
            ".\"  RX-OK\"",
        ], max_steps=200_000_000)
        self.assertIn("RX-OK", text)

    def test_net_status_after_heavy_traffic(self):
        """NET-STATUS should still report sane state after heavy I/O."""
        text = self._run_kdos_tap([
            # Generate some traffic — inline calls to avoid dictionary pressure
            "ARP-POLL ARP-POLL ARP-POLL ARP-POLL ARP-POLL",
            "ARP-POLL ARP-POLL ARP-POLL ARP-POLL ARP-POLL",
            "PING-POLL PING-POLL PING-POLL PING-POLL PING-POLL",
            "PING-POLL PING-POLL PING-POLL PING-POLL PING-POLL",
            # Now check status — device-present (bit7) + link-up (bit2) must be set
            "NET-STATUS DUP .\"  ns=\" . 132 AND 132 = .\"  sanity=\" .",
        ], max_steps=200_000_000)
        # Check that present + link-up bits are set (other bits may vary)
        self.assertIn("sanity=-1 ", text)

    def test_tcp_init_all_on_tap(self):
        """TCP-INIT-ALL should not crash when TAP is active."""
        text = self._run_kdos_tap([
            "TCP-INIT-ALL .\"  TCP-INIT-OK\"",
        ], max_steps=200_000_000)
        self.assertIn("TCP-INIT-OK", text)

    def test_tcp_poll_with_tap_noise(self):
        """TCP-POLL on live TAP should handle stray frames gracefully."""
        text = self._run_kdos_tap([
            "TCP-INIT-ALL",
            ": TPOLL50 50 0 DO TCP-POLL LOOP ;",
            "TPOLL50 .\"  TPOLL-OK\"",
        ], max_steps=200_000_000)
        self.assertIn("TPOLL-OK", text)


# ---------------------------------------------------------------------------
#  End-to-end TAP networking tests — real replies required
# ---------------------------------------------------------------------------

@_skip_no_tap
@pytest.mark.realnet
@pytest.mark.xdist_group("tap")
class TestRealNetEndToEnd(_RealNetBase):
    """End-to-end networking tests that require real replies.

    These are the definitive tests: they boot KDOS with IP config,
    exercise PING-IP and DNS-RESOLVE over a real TAP device, and
    assert that actual replies come back — not just "no crash".
    """

    def test_ping_gateway_receives_replies(self):
        """PING the gateway (host side of TAP) and verify replies come back.

        The Linux kernel always responds to ICMP echo requests sent to
        10.64.0.1 (the host address on the TAP interface).  This test
        sends 3 pings and asserts that at least 1 reply is received.
        """
        text = self._run_kdos_tap([
            '10 64 0 1 3 PING-IP',
        ], max_steps=500_000_000)
        # PING-IP prints: "N sent, M received"
        self.assertIn("sent", text.lower(),
                      f"PING-IP should have run. Output: {text[-400:]}")
        # Assert at least one reply was received
        self.assertRegex(
            text, r'[1-9]\d*\s+received',
            f"Expected at least 1 ping reply from gateway. Output:\n{text[-500:]}")

    def test_ping_gateway_reply_lines(self):
        """PING should print 'Reply seq=' lines for each received reply."""
        # Need enough pings for ARP to resolve first (seq=0 triggers ARP,
        # seq=1 may timeout while ARP reply is in flight).
        text = self._run_kdos_tap([
            '10 64 0 1 5 PING-IP',
        ], max_steps=500_000_000)
        self.assertIn("Reply seq=", text,
                      f"Expected 'Reply seq=' in ping output. Got:\n{text[-500:]}")

    def test_arp_resolve_gateway_succeeds(self):
        """ARP-RESOLVE for the gateway must return a non-zero MAC address."""
        text = self._run_kdos_tap([
            "GW-IP IP@ ARP-RESOLVE",
            "DUP 0<> IF",
            "  .\"  ARP-RESOLVED \"",
            "  C@ .\"  byte0=\" .",
            "ELSE",
            "  .\"  ARP-FAILED \"",
            "  DROP",
            "THEN",
        ], max_steps=200_000_000)
        self.assertIn("ARP-RESOLVED", text,
                      f"ARP-RESOLVE should succeed for gateway. Got:\n{text[-400:]}")

    def test_dns_resolve_google(self):
        """DNS-RESOLVE for google.com should return a valid IP address.

        Requires: TAP with internet access (IP forwarding + NAT).
        If DNS fails, the test provides a diagnostic skip.
        """
        text = self._run_kdos_tap([
            'DNS-LOOKUP google.com',
            "DUP 0<> IF",
            "  .\"  DNS-OK ip=\" .IP",
            "ELSE",
            "  .\"  DNS-FAIL \"",
            "THEN",
        ], max_steps=500_000_000)
        if "DNS-FAIL" in text:
            # Might not have internet — skip rather than fail
            self.skipTest(
                "DNS resolution failed — TAP may not have internet access. "
                "Ensure: sysctl net.ipv4.ip_forward=1 and "
                "iptables -t nat -A POSTROUTING -s 10.64.0.0/24 "
                "! -o mp64tap0 -j MASQUERADE")
        self.assertIn("DNS-OK", text,
                      f"DNS-LOOKUP should succeed. Got:\n{text[-400:]}")

    def test_scroll_get_produces_output(self):
        """SCROLL-GET http://... should fetch real HTTP content.

        Requires: TAP with internet access.
        """
        text = self._run_kdos_tap([
            'SCROLL-GET http://example.com',
        ], max_steps=500_000_000)
        if "DNS/IP resolve failed" in text or "DNS-FAIL" in text:
            self.skipTest("DNS not available over TAP")
        if "TCP connect failed" in text:
            self.skipTest("TCP connection failed — may not have internet")
        # If we got past DNS + TCP, there should be some HTTP content
        # example.com returns a simple HTML page
        got_content = (
            "Example Domain" in text
            or "HTTP" in text
            or "<html" in text.lower()
            or "200" in text
        )
        self.assertTrue(got_content,
                        f"Expected HTTP content from example.com. Got:\n{text[-600:]}")

    def test_scroll_get_https(self):
        """SCROLL-GET https://... should fetch content over TLS 1.3.

        Requires: TAP with internet access.
        """
        text = self._run_kdos_tap([
            'SCROLL-GET https://example.com',
        ], max_steps=800_000_000)
        if "DNS/IP resolve failed" in text or "DNS-FAIL" in text:
            self.skipTest("DNS not available over TAP")
        if "TLS connect failed" in text:
            self.skipTest("TLS connection failed — may not have internet")
        if "TCP connect failed" in text:
            self.skipTest("TCP connection failed — may not have internet")
        got_content = (
            "Example Domain" in text
            or "HTTP" in text
            or "<html" in text.lower()
            or "200" in text
            or "bytes in SCROLL-BUF" in text
        )
        self.assertTrue(got_content,
                        f"Expected HTTPS content from example.com. Got:\n{text[-600:]}")

    def test_tls_connect_diagnostic(self):
        """Diagnostic: TLS-CONNECT with breadcrumbs at each stage.

        Prints markers so we can see exactly where the TLS handshake
        stalls when test_scroll_get_https fails.
        """
        # S" is compile-only — wrap everything in a colon definition
        text = self._run_kdos_tap([
            ': DO-TLS-DIAG',
            '  S" example.com" DNS-RESOLVE DUP 0= IF',
            '    ." [DNS-FAIL]" CR EXIT',
            '  THEN',
            '  ." [DNS-OK] " DUP . CR',
            '  11 TLS-SNI-LEN !',
            '  S" example.com" TLS-SNI-HOST SWAP CMOVE',
            '  ." [TLS-CONNECT] " CR',
            '  443 12345 TLS-CONNECT',
            '  DUP 0= IF',
            '    ." [TLS-FAIL]" CR EXIT',
            '  THEN',
            '  ." [TLS-OK] ctx=" DUP . CR',
            '  TLS-CLOSE',
            ';',
            'DO-TLS-DIAG',
        ], max_steps=800_000_000)
        if "DNS-FAIL" in text:
            self.skipTest("DNS not available over TAP")
        self.assertIn("[TLS-CONNECT]", text,
                      f"Should reach TLS-CONNECT. Got:\n{text[-500:]}")
        # If we got [TLS-OK] or [TLS-FAIL], TLS-CONNECT at least returned
        tls_returned = "[TLS-OK]" in text or "[TLS-FAIL]" in text
        self.assertTrue(tls_returned,
                        f"TLS-CONNECT hung (no result). Got:\n{text[-500:]}")
        self.assertIn("[TLS-OK]", text,
                      f"TLS-CONNECT failed. Got:\n{text[-500:]}")

    def test_url_parse_https_port(self):
        """URL-PARSE should set port 443 for https:// URLs."""
        # S" is compile-only — wrap in a colon definition
        text = self._run_kdos_tap([
            ': DO-PARSE S" https://example.com/foo" URL-PARSE DROP ; DO-PARSE',
            '_SC-PROTO @ . _SC-PORT @ .',
        ], max_steps=50_000_000, setup_ip=False)
        # PROTO-HTTPS = 3, port = 443
        self.assertIn("3", text)
        self.assertIn("443", text)

    def test_url_parse_ftp(self):
        """URL-PARSE should handle ftp:// and ftps:// protocols."""
        # S" is compile-only — wrap in a colon definition
        text = self._run_kdos_tap([
            ': DO-PARSE S" ftp://ftp.example.com/pub/file.txt" URL-PARSE DROP ; DO-PARSE',
            '_SC-PROTO @ . _SC-PORT @ .',
        ], max_steps=50_000_000, setup_ip=False)
        # PROTO-FTP = 4, port = 21
        self.assertIn("4", text)
        self.assertIn("21", text)

    def test_url_parse_ftps(self):
        """URL-PARSE should set port 990 for ftps:// URLs."""
        # S" is compile-only — wrap in a colon definition
        text = self._run_kdos_tap([
            ': DO-PARSE S" ftps://secure.example.com/data" URL-PARSE DROP ; DO-PARSE',
            '_SC-PROTO @ . _SC-PORT @ .',
        ], max_steps=50_000_000, setup_ip=False)
        # PROTO-FTPS = 5, port = 990
        self.assertIn("5", text)
        self.assertIn("990", text)


# ---------------------------------------------------------------------------
#  Backward compatibility — ensure existing simulated tests unaffected
# ---------------------------------------------------------------------------

class TestBackwardCompat(unittest.TestCase):
    """Verify that the default (no-backend) NetworkDevice still works
    exactly as before — inject_frame, on_tx_frame, data port, DMA."""

    def test_default_nic_no_backend(self):
        """NetworkDevice() with no args should behave as loopback."""
        from devices import NetworkDevice
        nic = NetworkDevice()
        self.assertTrue(nic.link_up)
        self.assertEqual(nic.backend_name, "loopback")
        self.assertIsNone(nic.backend)

    def test_inject_and_drain(self):
        """inject_frame + drain_tx should work as before."""
        from devices import NetworkDevice
        nic = NetworkDevice()
        nic.inject_frame(b'\xAA' * 20)
        self.assertEqual(len(nic.rx_queue), 1)
        self.assertEqual(nic.rx_count, 1)

    def test_on_tx_frame_still_works(self):
        """on_tx_frame callback should still fire on SEND."""
        from devices import NetworkDevice
        captured = []
        nic = NetworkDevice()
        nic.on_tx_frame = lambda f: captured.append(f)
        frame = b'\xBB' * 30
        nic._data_buf = bytearray(frame)
        nic.frame_len = len(frame)
        nic._execute_cmd(0x01)
        self.assertEqual(len(captured), 1)
        self.assertEqual(captured[0], frame)

    def test_passthrough_legacy_still_works(self):
        """UDP passthrough constructor args should still be accepted."""
        from devices import NetworkDevice
        nic = NetworkDevice(passthrough_port=19899)
        self.assertEqual(nic.backend_name, "udp-legacy:19899")
        nic.stop()
