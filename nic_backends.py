"""
NIC Backends — Real and Simulated Network Interconnects
========================================================
Pluggable backends for the Megapad-64 emulated NIC.

Backend hierarchy:
  NICBackend          — abstract base
  ├─ LoopbackBackend  — in-process queues (unit tests)
  ├─ UDPBackend       — UDP tunnel between emulator instances
  └─ TAPBackend       — Linux TAP device (real L2 networking)

Usage:
  from nic_backends import TAPBackend
  backend = TAPBackend(tap_name="mp64tap0")
  sys = MegapadSystem(..., nic_backend=backend)

TAP setup (one-time, requires CAP_NET_ADMIN):
  sudo ip tuntap add dev mp64tap0 mode tap user $USER
  sudo ip link set mp64tap0 up
  sudo ip addr add 10.64.0.1/24 dev mp64tap0

  # Optional: masquerade for internet access
  sudo iptables -t nat -A POSTROUTING -s 10.64.0.0/24 -j MASQUERADE
  sudo sysctl net.ipv4.ip_forward=1
"""

from __future__ import annotations

import abc
import fcntl
import os
import select
import socket
import struct
import threading
import time
from collections import deque
from typing import Optional, Callable

# ── Constants ─────────────────────────────────────────────────────────
NIC_MTU = 1500

# Linux ioctl constants for TAP
TUNSETIFF   = 0x400454ca
IFF_TAP     = 0x0002
IFF_NO_PI   = 0x1000   # No packet info header (raw Ethernet frames)


# ══════════════════════════════════════════════════════════════════════
#  Abstract base
# ══════════════════════════════════════════════════════════════════════

class NICBackend(abc.ABC):
    """Abstract NIC backend.  Subclasses bridge emulated frames to/from
    some transport — loopback queues, UDP tunnels, or real TAP devices.
    """

    def __init__(self):
        # Callback invoked when a frame arrives from the wire.
        # Signature: on_rx_frame(frame: bytes)
        self.on_rx_frame: Optional[Callable[[bytes], None]] = None

    @abc.abstractmethod
    def send(self, frame: bytes) -> bool:
        """Transmit *frame* to the wire.  Returns True on success."""
        ...

    @abc.abstractmethod
    def start(self):
        """Begin listening for inbound frames (if applicable)."""
        ...

    @abc.abstractmethod
    def stop(self):
        """Shut down the backend and release resources."""
        ...

    @property
    @abc.abstractmethod
    def link_up(self) -> bool:
        """True if the backend considers the link operational."""
        ...

    @property
    def name(self) -> str:
        """Human-readable backend name for status display."""
        return type(self).__name__


# ══════════════════════════════════════════════════════════════════════
#  Loopback — in-process queues (default for unit tests)
# ══════════════════════════════════════════════════════════════════════

class LoopbackBackend(NICBackend):
    """Frames go nowhere — inject_frame() pushes into RX manually.

    This is the default backend when no real transport is configured.
    It preserves the existing behaviour: tests inject frames via
    ``nic.inject_frame()`` and inspect TX via ``on_tx_frame``.
    """

    def __init__(self):
        super().__init__()
        self._up = True

    def send(self, frame: bytes) -> bool:
        return True   # silently drop (tests use on_tx_frame callback)

    def start(self):
        pass

    def stop(self):
        self._up = False

    @property
    def link_up(self) -> bool:
        return self._up

    @property
    def name(self) -> str:
        return "loopback"


# ══════════════════════════════════════════════════════════════════════
#  UDP tunnel — existing passthrough, now as a proper backend
# ══════════════════════════════════════════════════════════════════════

class UDPBackend(NICBackend):
    """UDP tunnel: encapsulates raw Ethernet frames in UDP datagrams.

    Useful for connecting two emulator instances on the same host or
    across a LAN without requiring root privileges.
    """

    def __init__(self, bind_port: int,
                 peer_host: str = '127.0.0.1',
                 peer_port: Optional[int] = None):
        super().__init__()
        self._bind_port = bind_port
        self._peer_host = peer_host
        self._peer_port = peer_port or (bind_port + 1)
        self._sock: Optional[socket.socket] = None
        self._thread: Optional[threading.Thread] = None
        self._running = False
        self._error = False

    def send(self, frame: bytes) -> bool:
        if not self._sock:
            return False
        try:
            self._sock.sendto(frame[:NIC_MTU],
                              (self._peer_host, self._peer_port))
            return True
        except OSError:
            self._error = True
            return False

    def start(self):
        try:
            self._sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
            self._sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            self._sock.bind(('0.0.0.0', self._bind_port))
            self._sock.settimeout(0.1)
            self._running = True
            self._thread = threading.Thread(
                target=self._rx_loop, daemon=True, name="NIC-UDP-RX"
            )
            self._thread.start()
        except OSError:
            self._error = True

    def stop(self):
        self._running = False
        if self._sock:
            try:
                self._sock.close()
            except OSError:
                pass
            self._sock = None

    def _rx_loop(self):
        while self._running and self._sock:
            try:
                data, _addr = self._sock.recvfrom(NIC_MTU + 64)
                if data and self.on_rx_frame:
                    self.on_rx_frame(bytes(data[:NIC_MTU]))
            except socket.timeout:
                continue
            except OSError:
                break

    @property
    def link_up(self) -> bool:
        return self._running and not self._error

    @property
    def name(self) -> str:
        return f"udp:{self._bind_port}→{self._peer_host}:{self._peer_port}"


# ══════════════════════════════════════════════════════════════════════
#  TAP — real Layer-2 networking via Linux TAP device
# ══════════════════════════════════════════════════════════════════════

class TAPBackend(NICBackend):
    """Real networking via a Linux TAP interface.

    The TAP device must be pre-created with appropriate permissions:

        sudo ip tuntap add dev mp64tap0 mode tap user $USER
        sudo ip link set mp64tap0 up
        sudo ip addr add 10.64.0.1/24 dev mp64tap0

    The emulator writes/reads raw Ethernet frames to/from the TAP fd.
    This gives the KDOS networking stack access to a real L2 segment
    where ARP, ICMP, UDP, TCP, DHCP, and DNS all work for real.

    For internet access, enable IP forwarding and masquerading:

        sudo sysctl net.ipv4.ip_forward=1
        sudo iptables -t nat -A POSTROUTING -s 10.64.0.0/24 -j MASQUERADE
    """

    def __init__(self, tap_name: str = "mp64tap0"):
        super().__init__()
        self._tap_name = tap_name
        self._fd: Optional[int] = None
        self._thread: Optional[threading.Thread] = None
        self._running = False
        self._error = False
        self._error_msg: str = ""
        # Pipe for clean shutdown of select() loop
        self._stop_r: Optional[int] = None
        self._stop_w: Optional[int] = None
        # Stats
        self.tx_bytes: int = 0
        self.rx_bytes: int = 0
        self.tx_errors: int = 0

    def send(self, frame: bytes) -> bool:
        if self._fd is None:
            return False
        try:
            n = os.write(self._fd, frame[:NIC_MTU])
            self.tx_bytes += n
            return True
        except OSError as e:
            self.tx_errors += 1
            self._error_msg = str(e)
            return False

    def start(self):
        try:
            self._fd = self._open_tap(self._tap_name)
        except OSError as e:
            self._error = True
            self._error_msg = str(e)
            return

        self._stop_r, self._stop_w = os.pipe()
        self._running = True
        self._thread = threading.Thread(
            target=self._rx_loop, daemon=True,
            name=f"NIC-TAP-{self._tap_name}"
        )
        self._thread.start()

    def stop(self):
        self._running = False
        # Signal the select() to wake up
        if self._stop_w is not None:
            try:
                os.write(self._stop_w, b'\x00')
            except OSError:
                pass
        if self._thread:
            self._thread.join(timeout=2.0)
            self._thread = None
        if self._fd is not None:
            try:
                os.close(self._fd)
            except OSError:
                pass
            self._fd = None
        for fd in (self._stop_r, self._stop_w):
            if fd is not None:
                try:
                    os.close(fd)
                except OSError:
                    pass
        self._stop_r = self._stop_w = None

    def _rx_loop(self):
        """Read raw Ethernet frames from the TAP fd via select()."""
        while self._running and self._fd is not None:
            try:
                readable, _, _ = select.select(
                    [self._fd, self._stop_r], [], [], 0.5
                )
            except (OSError, ValueError):
                break

            if self._stop_r in readable:
                break  # shutdown signalled

            if self._fd in readable:
                try:
                    data = os.read(self._fd, NIC_MTU + 64)
                    if data:
                        frame = data[:NIC_MTU]
                        self.rx_bytes += len(frame)
                        if self.on_rx_frame:
                            self.on_rx_frame(bytes(frame))
                except OSError:
                    break

    @staticmethod
    def _open_tap(name: str) -> int:
        """Open /dev/net/tun and attach to the named TAP device.

        Returns the file descriptor for reading/writing raw Ethernet
        frames.  Raises OSError if the device can't be opened (e.g.
        the TAP doesn't exist or insufficient permissions).
        """
        fd = os.open("/dev/net/tun", os.O_RDWR)
        try:
            # struct ifreq: 16 bytes name + 2 bytes flags + padding
            ifr = struct.pack("16sH", name.encode("ascii"), IFF_TAP | IFF_NO_PI)
            fcntl.ioctl(fd, TUNSETIFF, ifr)
        except Exception:
            os.close(fd)
            raise
        # Set non-blocking for the select() loop
        flags = fcntl.fcntl(fd, fcntl.F_GETFL)
        fcntl.fcntl(fd, fcntl.F_SETFL, flags | os.O_NONBLOCK)
        return fd

    @property
    def link_up(self) -> bool:
        return self._running and self._fd is not None and not self._error

    @property
    def name(self) -> str:
        s = f"tap:{self._tap_name}"
        if self._error:
            s += f" [ERROR: {self._error_msg}]"
        return s

    # -- Diagnostics --

    def stats(self) -> dict:
        return {
            "tap": self._tap_name,
            "link_up": self.link_up,
            "tx_bytes": self.tx_bytes,
            "rx_bytes": self.rx_bytes,
            "tx_errors": self.tx_errors,
            "error": self._error_msg or None,
        }


# ══════════════════════════════════════════════════════════════════════
#  Helpers
# ══════════════════════════════════════════════════════════════════════

def tap_available(tap_name: str = "mp64tap0") -> bool:
    """Test whether the named TAP device can be opened.

    Returns True if /dev/net/tun exists and the TAP interface is
    accessible by the current user.  Does not leave any fd open.
    """
    if not os.path.exists("/dev/net/tun"):
        return False
    try:
        fd = TAPBackend._open_tap(tap_name)
        os.close(fd)
        return True
    except OSError:
        return False


def setup_tap(tap_name: str = "mp64tap0",
              host_ip: str = "10.64.0.1",
              prefix_len: int = 24) -> str:
    """Return the shell commands to create and configure a TAP device.

    Does NOT execute them — the user should run them with sudo.
    """
    user = os.environ.get("USER", "nobody")
    return (
        f"sudo ip tuntap add dev {tap_name} mode tap user {user}\n"
        f"sudo ip link set {tap_name} up\n"
        f"sudo ip addr add {host_ip}/{prefix_len} dev {tap_name}\n"
        f"\n"
        f"# For internet access (optional):\n"
        f"sudo sysctl -w net.ipv4.ip_forward=1\n"
        f"sudo iptables -t nat -A POSTROUTING -s {host_ip}/{prefix_len} "
        f"! -o {tap_name} -j MASQUERADE\n"
    )
