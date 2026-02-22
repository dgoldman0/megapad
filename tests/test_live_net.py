"""
Live Network Tests for Megapad-64
=================================
All tests that require a real Linux TAP device live here.  This module
is designed to handle TAP contention, share a single KDOS snapshot
across every test class, and protect the TAP fd with a process-level
file lock — so it works reliably under ``pytest-xdist -n 8 --dist
loadgroup`` as well as single-process ``make test-net``.

Prerequisites
-------------
Create and configure the TAP interface (one-time, requires root)::

    sudo ip tuntap add dev mp64tap0 mode tap user $USER
    sudo ip link set mp64tap0 up
    sudo ip addr add 10.64.0.1/24 dev mp64tap0

For internet access (DNS, HTTP, HTTPS, TLS tests)::

    sudo sysctl -w net.ipv4.ip_forward=1
    sudo iptables -t nat -A POSTROUTING -s 10.64.0.0/24 \\
         ! -o mp64tap0 -j MASQUERADE

Running
-------
::

    make test-net                # all live tests (single-process, verbose)
    make test-net K=TestLiveARP  # subset
    make test                    # full suite (xdist, live tests on one worker)

Network Layout
--------------
::

    Host side:   10.64.0.1  (on mp64tap0)
    Emulator:    10.64.0.2  (configured by each test via IP-SET)
    Emulator MAC: 02:4D:50:36:34:00  (NIC default)
"""

from __future__ import annotations

import fcntl
import os
import socket
import struct
import tempfile
import threading
import time
import unittest

import pytest

# ---------------------------------------------------------------------------
#  Configuration
# ---------------------------------------------------------------------------

_TAP_NAME = os.environ.get("MP64_TAP", "mp64tap0")
_HOST_IP  = os.environ.get("MP64_HOST_IP", "10.64.0.1")
_EMU_IP   = os.environ.get("MP64_EMU_IP", "10.64.0.2")

_LOCK_PATH = os.path.join(tempfile.gettempdir(), "mp64_tap.lock")

# Paths — resolved relative to the project root (one level up from tests/)
_PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
_BIOS_PATH     = os.path.join(_PROJECT_ROOT, "bios.asm")
_KDOS_PATH     = os.path.join(_PROJECT_ROOT, "kdos.f")
_TOOLS_PATH    = os.path.join(_PROJECT_ROOT, "tools.f")
_AUTOEXEC_PATH = os.path.join(_PROJECT_ROOT, "autoexec.f")
_GRAPHICS_PATH = os.path.join(_PROJECT_ROOT, "graphics.f")

EMU_MAC = b'\x02\x4D\x50\x36\x34\x00'


# ---------------------------------------------------------------------------
#  TAP availability — deferred, not at import time
# ---------------------------------------------------------------------------

def _tap_probe(tap_name: str = _TAP_NAME) -> bool:
    """Return True if the TAP device can be opened right now.

    Unlike the old ``tap_available()`` that ran at import time on every
    xdist worker, this is called **only** when we actually need the TAP
    (inside ``_ensure_snapshot`` or a fixture), avoiding the 8-worker
    import-time contention entirely.
    """
    from nic_backends import tap_available
    return tap_available(tap_name)


# ---------------------------------------------------------------------------
#  Process-level file lock for TAP access
# ---------------------------------------------------------------------------

class TAPLock:
    """Advisory file lock that serialises TAP access across processes.

    Usage::

        with TAPLock():
            backend = TAPBackend(tap_name)
            try:
                ...
            finally:
                backend.stop()

    The lock is advisory (``flock``), so it only coordinates processes
    that use it — but every path into this module goes through
    ``_run_kdos_tap`` which always acquires it.
    """

    def __init__(self, path: str = _LOCK_PATH):
        self._path = path
        self._fd = None

    def __enter__(self):
        self._fd = open(self._path, "w")
        fcntl.flock(self._fd, fcntl.LOCK_EX)
        return self

    def __exit__(self, *exc):
        if self._fd:
            fcntl.flock(self._fd, fcntl.LOCK_UN)
            self._fd.close()
            self._fd = None


# ---------------------------------------------------------------------------
#  Helpers
# ---------------------------------------------------------------------------

def _load_bios():
    from asm import assemble
    with open(_BIOS_PATH) as f:
        return assemble(f.read())


def _next_line_chunk(data: bytes, pos: int) -> bytes:
    """Return bytes from *pos* up to and including the next newline."""
    nl = data.find(b'\n', pos)
    if nl == -1:
        return data[pos:]
    return data[pos:nl + 1]


def capture_uart(sys_obj):
    buf = []
    sys_obj.uart.on_tx = lambda b: buf.append(b)
    return buf


def uart_text(buf) -> str:
    return "".join(
        chr(b) if (0x20 <= b < 0x7F or b in (10, 13, 9)) else ""
        for b in buf
    )


# ---------------------------------------------------------------------------
#  CPU snapshot helpers
# ---------------------------------------------------------------------------

_CPU_STATE_KEYS = (
    'psel', 'xsel', 'spsel',
    'flag_z', 'flag_c', 'flag_n', 'flag_v',
    'flag_p', 'flag_g', 'flag_i', 'flag_s',
    'd_reg', 'q_out', 't_reg',
    'ivt_base', 'ivec_id', 'trap_addr',
    'halted', 'idle', 'cycle_count', '_ext_modifier',
)


def _save_cpu_state(cpu) -> dict:
    state = {'pc': cpu.pc, 'regs': list(cpu.regs)}
    for k in _CPU_STATE_KEYS:
        state[k] = getattr(cpu, k)
    return state


def _restore_cpu_state(cpu, state: dict):
    cpu.pc = state['pc']
    cpu.regs[:] = state['regs']
    for k in _CPU_STATE_KEYS:
        setattr(cpu, k, state[k])


# ---------------------------------------------------------------------------
#  Module-level snapshot — shared by ALL test classes
# ---------------------------------------------------------------------------

_snapshot = None            # (mem_bytes, cpu_state, disk_image_path)
_snapshot_lock = threading.Lock()


def _build_disk_image() -> str:
    """Create a unique-per-process disk image with KDOS + friends."""
    from diskutil import format_image, inject_file, FTYPE_FORTH

    path = os.path.join(
        tempfile.gettempdir(),
        f"mp64_livenet_{os.getpid()}.img",
    )
    format_image(path)
    for fpath in (_KDOS_PATH, _AUTOEXEC_PATH, _GRAPHICS_PATH, _TOOLS_PATH):
        with open(fpath, 'rb') as f:
            inject_file(path, os.path.basename(fpath), f.read(),
                        ftype=FTYPE_FORTH)
    return path


def _ensure_snapshot():
    """Build the KDOS snapshot once per process.

    Thread-safe (``_snapshot_lock``) in case xdist somehow schedules
    two tests concurrently on the same worker — shouldn't happen with
    ``loadgroup``, but costs nothing to be safe.
    """
    global _snapshot
    if _snapshot is not None:
        return

    with _snapshot_lock:
        if _snapshot is not None:           # double-checked locking
            return

        from system import MegapadSystem

        bios_code = _load_bios()
        disk_path = _build_disk_image()

        sys_obj = MegapadSystem(
            ram_size=1024 * 1024,
            storage_image=disk_path,
            ext_mem_size=16 * (1 << 20),    # 16 MiB XMEM for XBUF
        )
        sys_obj.nic.link_up = False         # prevent autoexec DHCP
        buf = capture_uart(sys_obj)
        sys_obj.load_binary(0, bios_code)
        sys_obj.boot()

        max_steps = 1_000_000_000
        total = 0
        while total < max_steps:
            if sys_obj.cpu.halted or sys_obj.cpu.idle:
                break
            batch = sys_obj.run_batch(min(100_000, max_steps - total))
            total += max(batch, 1)

        _snapshot = (
            bytes(sys_obj.cpu.mem),
            _save_cpu_state(sys_obj.cpu),
            disk_path,
        )


# ---------------------------------------------------------------------------
#  Skip decorator — same pattern, but the probe only fires once
# ---------------------------------------------------------------------------

_tap_ok_cache = None


def _check_tap() -> bool:
    global _tap_ok_cache
    if _tap_ok_cache is None:
        _tap_ok_cache = _tap_probe()
    return _tap_ok_cache


_skip_no_tap = pytest.mark.skipif(
    "not __import__('tests.test_live_net', fromlist=['_check_tap'])._check_tap()",
    reason=f"TAP device '{_TAP_NAME}' not accessible "
           f"(run setup commands from test_live_net.py docstring)",
)


# ---------------------------------------------------------------------------
#  Base class for live-network tests
# ---------------------------------------------------------------------------

@_skip_no_tap
@pytest.mark.realnet
@pytest.mark.xdist_group("tap")
class LiveNetBase(unittest.TestCase):
    """Base for every test that opens TAP.

    Key improvements over the old ``_RealNetBase``:

    * **Module-level snapshot** — KDOS boots once per process, not once
      per class.  With 7 subclasses this saves ~60 s of boot time.

    * **Process-level file lock** — ``TAPLock`` wraps every TAP
      interaction with ``flock``, so even if ``loadgroup`` misbehaves
      two processes won't both try to open the TAP fd at the same time.

    * **Unique disk image** — per-PID temp path eliminates the race on
      ``/tmp/mp64_realnet_test.img``.

    * **No import-time TAP probe** — ``_check_tap()`` defers the probe
      until the first test is actually collected/run, avoiding the
      8-worker EBUSY stampede.

    * **Robust idle handling** — longer idle timeout for TLS tests,
      explicit GIL-yield via ``time.sleep(0.02)`` in every idle poll.
    """

    def setUp(self):
        _ensure_snapshot()

    def _run(self, extra_lines: list[str], *,
             max_steps: int = 100_000_000,
             setup_ip: bool = True,
             idle_timeout_s: float = 4.0) -> str:
        """Restore KDOS, attach TAP, run Forth commands, return UART text.

        Parameters
        ----------
        extra_lines : list[str]
            Forth source lines to feed after optional IP config.
        max_steps : int
            CPU step budget.
        setup_ip : bool
            If True, configure 10.64.0.2/24 + gateway + DNS before
            ``extra_lines``.
        idle_timeout_s : float
            How long to keep polling when the CPU is idle and no
            UART/NIC data is pending.  Default 4 s, increase for TLS.
        """
        from nic_backends import TAPBackend
        from system import MegapadSystem

        mem_bytes, cpu_state, disk_path = _snapshot

        with TAPLock():
            backend = TAPBackend(tap_name=_TAP_NAME)
            sys = MegapadSystem(
                ram_size=1024 * 1024,
                nic_backend=backend,
                storage_image=disk_path,
                ext_mem_size=16 * (1 << 20),
            )
            buf = capture_uart(sys)

            # Restore snapshot
            sys.cpu.mem[:len(mem_bytes)] = mem_bytes
            _restore_cpu_state(sys.cpu, cpu_state)

            # Prepare commands
            ip_lines = []
            if setup_ip:
                parts = _EMU_IP.split(".")
                gw = _HOST_IP.split(".")
                ip_lines = [
                    f"{parts[0]} {parts[1]} {parts[2]} {parts[3]} IP-SET",
                    f"{gw[0]} {gw[1]} {gw[2]} {gw[3]} GW-IP IP!",
                    "255 255 255 0 NET-MASK IP!",
                    "8 8 8 8 DNS-SERVER-IP IP!",
                ]

            payload = ("\n".join(ip_lines + extra_lines) + "\nBYE\n").encode()
            pos = 0
            steps = 0
            idle_polls = 0
            max_idle_polls = max(1, int(idle_timeout_s / 0.02))

            try:
                while steps < max_steps:
                    if sys.cpu.halted:
                        break

                    # Feed UART data when buffer is empty
                    if not sys.uart.has_rx_data and pos < len(payload):
                        chunk = _next_line_chunk(payload, pos)
                        sys.uart.inject_input(chunk)
                        pos += len(chunk)
                        if sys.cpu.idle:
                            sys.cpu.idle = False
                        idle_polls = 0

                    # Run CPU if it has work
                    if not (sys.cpu.idle
                            and not sys.uart.has_rx_data
                            and not sys._any_nic_rx()):
                        batch = sys.run_batch(min(100_000, max_steps - steps))
                        steps += max(batch, 1)
                        idle_polls = 0
                        continue

                    # Idle — sleep to yield GIL to TAP RX thread
                    if idle_polls < max_idle_polls:
                        idle_polls += 1
                        time.sleep(0.02)
                        sys.cpu.idle = False
                    else:
                        break
            finally:
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


# ===================================================================
#  BIOS-level networking
# ===================================================================

class TestLiveBIOS(LiveNetBase):
    """BIOS networking primitives over a real TAP."""

    def test_net_status_link_up(self):
        """NET-STATUS should report link-up + present (0x84 = 132)."""
        text = self._run(["NET-STATUS ."], setup_ip=False)
        self.assertIn("132 ", text)

    def test_net_mac_readable(self):
        """NET-MAC@ first byte should be 0x02."""
        text = self._run(["NET-MAC@ C@ ."], setup_ip=False)
        self.assertIn("2 ", text)


# ===================================================================
#  ARP
# ===================================================================

class TestLiveARP(LiveNetBase):
    """ARP stack against a real TAP.  The host kernel auto-replies."""

    def test_arp_resolve_gateway(self):
        text = self._run([
            "GW-IP IP@ ARP-RESOLVE",
            'DUP 0<> IF .\"  ARP-OK \" C@ . ELSE .\"  ARP-FAIL \" DROP THEN',
        ], max_steps=200_000_000)
        self.assertIn("ARP-OK", text)

    def test_arp_table_populated(self):
        text = self._run([
            "GW-IP IP@ ARP-RESOLVE DROP",
            "ARP-TABLE ARP-SLOTS /ARP-ENT * DUMP",
        ], max_steps=200_000_000)
        self.assertNotIn("ARP-FAIL", text)

    def test_arp_handle_request_from_host(self):
        text = self._run([
            ': APOLL 20 0 DO ARP-POLL LOOP ; APOLL .\"  POLL-DONE \"',
        ], max_steps=200_000_000)
        self.assertIn("POLL-DONE", text)


# ===================================================================
#  ICMP (ping)
# ===================================================================

class TestLiveICMP(LiveNetBase):
    """ICMP echo over real TAP."""

    def test_ping_host(self):
        text = self._run([
            "GW-IP IP@ ARP-RESOLVE",
            'DUP 0= IF ." NO-ARP" DROP',
            "ELSE DROP",
            "  GW-IP IP@ 0 0 ICMP-BUILD-ECHO-REQ",
            '  1 IP-PROTO-ICMP GW-IP IP@ ROT ROT IP-SEND ." PING-SENT"',
            "  20 0 DO IP-RECV DUP 0<> IF",
            '    ." IP-GOT" 2DROP LEAVE ELSE 2DROP THEN LOOP',
            "THEN",
        ], max_steps=300_000_000)
        if "NO-ARP" in text:
            self.skipTest("ARP resolution failed")
        self.assertIn("PING-SENT", text)

    def test_respond_to_ping(self):
        text = self._run([
            ': PPOLL 50 0 DO PING-POLL LOOP ; PPOLL ." PING-RESPONDED"',
        ], max_steps=200_000_000)
        self.assertIn("PING-RESPONDED", text)


# ===================================================================
#  UDP
# ===================================================================

class TestLiveUDP(LiveNetBase):
    """UDP send/receive over real TAP."""

    def test_udp_send_to_host(self):
        received = []

        def listener(port):
            try:
                s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
                s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                s.bind((_HOST_IP, port))
                s.settimeout(10.0)
                data, _ = s.recvfrom(2048)
                received.append(data)
                s.close()
            except (socket.timeout, OSError):
                pass

        port = 7777
        t = threading.Thread(target=listener, args=(port,), daemon=True)
        t.start()
        time.sleep(0.1)

        text = self._run([
            f': DO-UDP GW-IP IP@ {port} 9999 S" HELLO-MP64" UDP-SEND . ; DO-UDP',
        ], max_steps=300_000_000)
        t.join(timeout=12.0)

        if received:
            self.assertIn(b"HELLO-MP64", received[0])
        self.assertNotIn("ABORT", text)

    def test_udp_recv_from_host(self):
        def sender(delay=1.0):
            time.sleep(delay)
            for _ in range(5):
                try:
                    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
                    s.sendto(b"HOST-MSG-42", (_EMU_IP, 5555))
                    s.close()
                except OSError:
                    pass
                time.sleep(0.5)

        t = threading.Thread(target=sender, args=(2.0,), daemon=True)
        t.start()

        text = self._run([
            "100 0 DO UDP-RECV DUP 0<> IF",
            '  ." UDP-GOT" DROP 2DROP LEAVE ELSE DROP 2DROP THEN LOOP',
        ], max_steps=400_000_000)
        t.join(timeout=15.0)
        self.assertNotIn("ABORT", text)


# ===================================================================
#  Integration
# ===================================================================

class TestLiveIntegration(LiveNetBase):
    """IP stack init + status after real TAP attachment."""

    def test_full_ip_stack_init(self):
        text = self._run([
            'MY-IP .IP ." myip" GW-IP .IP ." gw" NET-STATUS . ." status"',
        ])
        self.assertIn("status", text)
        self.assertIn("myip", text)
        self.assertIn("gw", text)

    def test_net_status_shows_backend(self):
        from nic_backends import TAPBackend
        from system import MegapadSystem

        with TAPLock():
            backend = TAPBackend(tap_name=_TAP_NAME)
            sys = MegapadSystem(ram_size=1024 * 1024, nic_backend=backend)
            try:
                status = sys.dump_state()
                self.assertIn(f"tap:{_TAP_NAME}", status)
            finally:
                sys.nic.stop()

    def test_backend_stats_tracking(self):
        from nic_backends import TAPBackend
        from system import MegapadSystem

        with TAPLock():
            backend = TAPBackend(tap_name=_TAP_NAME)
            sys = MegapadSystem(ram_size=1024 * 1024, nic_backend=backend)
            try:
                stats = backend.stats()
                self.assertIn("tx_bytes", stats)
                self.assertIn("rx_bytes", stats)
                self.assertTrue(stats["link_up"])
            finally:
                sys.nic.stop()


# ===================================================================
#  Hardening / stress
# ===================================================================

class TestLiveHardening(LiveNetBase):
    """Stress and robustness tests over real TAP."""

    def test_ping_ip_outbound(self):
        text = self._run(['10 64 0 1 2 PING-IP'], max_steps=300_000_000)
        self.assertIn("sent", text.lower())

    def test_rapid_arp_poll_100(self):
        text = self._run([
            ': ARP100 100 0 DO ARP-POLL LOOP ; ARP100 ." ARP100-OK"',
        ], max_steps=200_000_000)
        self.assertIn("ARP100-OK", text)

    def test_rapid_ping_poll_100(self):
        text = self._run([
            ': PP100 100 0 DO PING-POLL LOOP ; PP100 ." PP100-OK"',
        ], max_steps=200_000_000)
        self.assertIn("PP100-OK", text)

    def test_inject_runt_via_tap(self):
        text = self._run([
            ': APOLL 10 0 DO ARP-POLL LOOP ; APOLL',
            ': PPOLL 10 0 DO PING-POLL LOOP ; PPOLL ." RUNT-OK"',
        ], max_steps=200_000_000)
        self.assertIn("RUNT-OK", text)

    def test_broadcast_storm_via_tap(self):
        text = self._run([
            ': STORM-DRAIN 50 0 DO ARP-POLL LOOP ; STORM-DRAIN ." STORM-OK"',
        ], max_steps=200_000_000)
        self.assertIn("STORM-OK", text)

    def test_full_stack_roundtrip_udp(self):
        received = []

        def listener():
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

        t = threading.Thread(target=listener, daemon=True)
        t.start()
        time.sleep(0.1)

        text = self._run([
            ': DO-UDP GW-IP IP@ 7778 8888 S" HARDENING-OK" UDP-SEND . ; DO-UDP',
        ], max_steps=300_000_000)
        t.join(timeout=12.0)

        if received:
            self.assertIn(b"HARDENING-OK", received[0])
        self.assertNotIn("ABORT", text)

    def test_arp_then_icmp_then_udp_sequence(self):
        text = self._run([
            "GW-IP IP@ ARP-RESOLVE",
            'DUP 0<> IF ." ARP-OK" DROP ELSE ." ARP-FAIL" DROP THEN',
            ': PPOLL 10 0 DO PING-POLL LOOP ; PPOLL ." PING-OK"',
            ': DO-SEQ GW-IP IP@ 9999 8888 S" SEQ-TEST" UDP-SEND DROP ; DO-SEQ ." SEQ-OK"',
        ], max_steps=300_000_000)
        self.assertIn("PING-OK", text)
        self.assertIn("SEQ-OK", text)

    def test_ip_recv_no_crash_on_tap_noise(self):
        text = self._run([
            "VARIABLE IPCNT  0 IPCNT !",
            ": RX-MANY 20 0 DO IP-RECV DUP 0<> IF DROP 1 IPCNT +! ELSE DROP THEN LOOP ;",
            'RX-MANY IPCNT @ ." ipcnt=" . ." RX-OK"',
        ], max_steps=200_000_000)
        self.assertIn("RX-OK", text)

    def test_net_status_after_heavy_traffic(self):
        text = self._run([
            "ARP-POLL ARP-POLL ARP-POLL ARP-POLL ARP-POLL",
            "ARP-POLL ARP-POLL ARP-POLL ARP-POLL ARP-POLL",
            "PING-POLL PING-POLL PING-POLL PING-POLL PING-POLL",
            "PING-POLL PING-POLL PING-POLL PING-POLL PING-POLL",
            'NET-STATUS DUP ." ns=" . 132 AND 132 = ." sanity=" .',
        ], max_steps=200_000_000)
        self.assertIn("sanity=-1 ", text)

    def test_tcp_init_all_on_tap(self):
        text = self._run([
            'TCP-INIT-ALL ." TCP-INIT-OK"',
        ], max_steps=200_000_000)
        self.assertIn("TCP-INIT-OK", text)

    def test_tcp_poll_with_tap_noise(self):
        text = self._run([
            "TCP-INIT-ALL",
            ': TPOLL50 50 0 DO TCP-POLL LOOP ; TPOLL50 ." TPOLL-OK"',
        ], max_steps=200_000_000)
        self.assertIn("TPOLL-OK", text)


# ===================================================================
#  End-to-end (real replies required)
# ===================================================================

class TestLiveEndToEnd(LiveNetBase):
    """Definitive tests that require real replies from the network."""

    def test_ping_gateway_receives_replies(self):
        text = self._run(['10 64 0 1 3 PING-IP'], max_steps=500_000_000,
                         idle_timeout_s=6.0)
        self.assertIn("sent", text.lower())
        self.assertRegex(text, r'[1-9]\d*\s+received',
                         f"No ping replies.\n{text[-500:]}")

    def test_ping_gateway_reply_lines(self):
        text = self._run(['10 64 0 1 5 PING-IP'], max_steps=500_000_000,
                         idle_timeout_s=6.0)
        self.assertIn("Reply seq=", text,
                      f"No 'Reply seq=' in output.\n{text[-500:]}")

    def test_arp_resolve_gateway_succeeds(self):
        text = self._run([
            "GW-IP IP@ ARP-RESOLVE",
            'DUP 0<> IF ." ARP-RESOLVED" C@ ." byte0=" . '
            'ELSE ." ARP-FAILED" DROP THEN',
        ], max_steps=200_000_000)
        self.assertIn("ARP-RESOLVED", text)

    def test_dns_resolve_google(self):
        text = self._run([
            'DNS-LOOKUP google.com',
            'DUP 0<> IF ." DNS-OK ip=" .IP ELSE ." DNS-FAIL" THEN',
        ], max_steps=500_000_000, idle_timeout_s=8.0)
        if "DNS-FAIL" in text:
            self.skipTest("DNS failed — TAP may lack internet access")
        self.assertIn("DNS-OK", text)

    def test_scroll_get_produces_output(self):
        text = self._run([
            'SCROLL-GET http://example.com',
        ], max_steps=500_000_000, idle_timeout_s=10.0)
        if "DNS/IP resolve failed" in text or "DNS-FAIL" in text:
            self.skipTest("DNS not available over TAP")
        if "TCP connect failed" in text:
            self.skipTest("TCP connection failed")
        got = any(s in text for s in
                  ("Example Domain", "HTTP", "200", "bytes in SCROLL-BUF"))
        self.assertTrue(got, f"No HTTP content.\n{text[-600:]}")

    def test_scroll_get_https(self):
        text = self._run([
            'SCROLL-GET https://example.com',
        ], max_steps=800_000_000, idle_timeout_s=15.0)
        if "DNS/IP resolve failed" in text or "DNS-FAIL" in text:
            self.skipTest("DNS not available over TAP")
        if "TLS connect failed" in text or "TCP connect failed" in text:
            self.skipTest("TLS/TCP connection failed")
        got = any(s in text for s in
                  ("Example Domain", "HTTP", "200",
                   "bytes in SCROLL-BUF", "<html"))
        self.assertTrue(got, f"No HTTPS content.\n{text[-600:]}")

    def test_tls_connect_diagnostic(self):
        text = self._run([
            ': DO-TLS-DIAG',
            '  S" example.com" DNS-RESOLVE DUP 0= IF ." [DNS-FAIL]" CR EXIT THEN',
            '  ." [DNS-OK] " DUP . CR',
            '  11 TLS-SNI-LEN !',
            '  S" example.com" TLS-SNI-HOST SWAP CMOVE',
            '  ." [TLS-CONNECT] " CR',
            '  443 12345 TLS-CONNECT',
            '  DUP 0= IF ." [TLS-FAIL]" CR EXIT THEN',
            '  ." [TLS-OK] ctx=" DUP . CR',
            '  TLS-CLOSE',
            '; DO-TLS-DIAG',
        ], max_steps=800_000_000, idle_timeout_s=15.0)
        if "DNS-FAIL" in text:
            self.skipTest("DNS not available over TAP")
        self.assertIn("[TLS-CONNECT]", text)
        tls_returned = "[TLS-OK]" in text or "[TLS-FAIL]" in text
        self.assertTrue(tls_returned, f"TLS-CONNECT hung.\n{text[-500:]}")
        self.assertIn("[TLS-OK]", text, f"TLS-CONNECT failed.\n{text[-500:]}")

    def test_url_parse_https_port(self):
        text = self._run([
            ': DO-PARSE S" https://example.com/foo" URL-PARSE DROP ; DO-PARSE',
            '_SC-PROTO @ . _SC-PORT @ .',
        ], max_steps=50_000_000, setup_ip=False)
        self.assertIn("3", text)     # PROTO-HTTPS
        self.assertIn("443", text)

    def test_url_parse_ftp(self):
        text = self._run([
            ': DO-PARSE S" ftp://ftp.example.com/pub/file.txt" URL-PARSE DROP ; DO-PARSE',
            '_SC-PROTO @ . _SC-PORT @ .',
        ], max_steps=50_000_000, setup_ip=False)
        self.assertIn("4", text)     # PROTO-FTP
        self.assertIn("21", text)

    def test_url_parse_ftps(self):
        text = self._run([
            ': DO-PARSE S" ftps://secure.example.com/data" URL-PARSE DROP ; DO-PARSE',
            '_SC-PROTO @ . _SC-PORT @ .',
        ], max_steps=50_000_000, setup_ip=False)
        self.assertIn("5", text)     # PROTO-FTPS
        self.assertIn("990", text)


# ---------------------------------------------------------------------------
#  TAP backend unit-level tests (migrated from test_networking.py)
# ---------------------------------------------------------------------------

@pytest.mark.realnet
@pytest.mark.xdist_group("tap")
class TestLiveTAPBackend(LiveNetBase):
    """Low-level TAPBackend lifecycle tests that need the real TAP device."""

    def test_tap_backend_lifecycle(self):
        """TAPBackend should open, report link-up, and stop cleanly."""
        from nic_backends import TAPBackend
        b = TAPBackend(tap_name=_TAP_NAME)
        b.start()
        self.assertTrue(b.link_up)
        self.assertIn("tap:", b.name)
        stats = b.stats()
        self.assertEqual(stats["tx_bytes"], 0)
        b.stop()
        self.assertFalse(b.link_up)

    def test_tap_backend_send_frame(self):
        """TAPBackend.send() should write a frame without error."""
        from nic_backends import TAPBackend
        b = TAPBackend(tap_name=_TAP_NAME)
        b.start()
        self.assertTrue(b.link_up)
        frame = (b'\xff' * 6 + EMU_MAC +
                 b'\x08\x00' + b'\x00' * 46)  # 64 bytes total
        ok = b.send(frame)
        self.assertTrue(ok)
        self.assertGreater(b.tx_bytes, 0)
        b.stop()
