"""
NIC Backends — Real and Simulated Network Interconnects
========================================================
Pluggable backends for the Megapad-64 emulated NIC.

Backend hierarchy:
  NICBackend          — abstract base
  ├─ LoopbackBackend  — in-process queues (unit tests)
  ├─ UDPBackend       — UDP tunnel between emulator instances
  ├─ TAPBackend       — Linux TAP device (real L2 networking)
  └─ SlirpBackend     — user-mode network through libslirp, no
                        administrator rights needed

backend_from_spec() makes one from a server's --nic option: "slirp" or
"tap:IFNAME".

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
IP_MTU = 1500
NIC_MAX_FRAME = 14 + IP_MTU
# Compatibility alias for callers that use MTU in its L3 sense.  Raw frame
# transport uses NIC_MAX_FRAME throughout this module.
NIC_MTU = IP_MTU

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
        return 0 < len(frame) <= NIC_MAX_FRAME

    def start(self):
        self._up = True

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
        if not frame or len(frame) > NIC_MAX_FRAME:
            self._error = True
            return False
        try:
            self._sock.sendto(frame,
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
                data, _addr = self._sock.recvfrom(NIC_MAX_FRAME + 1)
                if len(data) > NIC_MAX_FRAME:
                    self._error = True
                    continue
                if data and self.on_rx_frame:
                    self.on_rx_frame(bytes(data))
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
        self.tx_frames: int = 0
        self.rx_frames: int = 0
        self.tx_errors: int = 0
        self.first_tx_frame: Optional[bytes] = None
        self.first_rx_frame: Optional[bytes] = None
        self.last_tx_frame: Optional[bytes] = None
        self.last_rx_frame: Optional[bytes] = None
        self.frame_trace: deque[tuple[str, bytes]] = deque(maxlen=32)

    def send(self, frame: bytes) -> bool:
        if self._fd is None:
            return False
        if not frame or len(frame) > NIC_MAX_FRAME:
            self.tx_errors += 1
            self._error_msg = "Ethernet frame exceeds 1514-byte no-FCS limit"
            return False
        try:
            payload = bytes(frame)
            n = os.write(self._fd, payload)
            self.tx_bytes += n
            self.tx_frames += 1
            if self.first_tx_frame is None:
                self.first_tx_frame = payload
            self.last_tx_frame = payload
            self.frame_trace.append(("tx", payload))
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
                    data = os.read(self._fd, NIC_MAX_FRAME + 1)
                    if data:
                        if len(data) > NIC_MAX_FRAME:
                            self._error_msg = (
                                "received Ethernet frame exceeds 1514-byte "
                                "no-FCS limit"
                            )
                            continue
                        frame = data
                        self.rx_bytes += len(frame)
                        self.rx_frames += 1
                        if self.first_rx_frame is None:
                            self.first_rx_frame = bytes(frame)
                        self.last_rx_frame = bytes(frame)
                        self.frame_trace.append(("rx", bytes(frame)))
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
            "tx_frames": self.tx_frames,
            "tx_bytes": self.tx_bytes,
            "rx_frames": self.rx_frames,
            "rx_bytes": self.rx_bytes,
            "tx_errors": self.tx_errors,
            "first_tx_hex": self._frame_prefix(self.first_tx_frame),
            "first_rx_hex": self._frame_prefix(self.first_rx_frame),
            "last_tx_hex": self._frame_prefix(self.last_tx_frame),
            "last_rx_hex": self._frame_prefix(self.last_rx_frame),
            "frame_trace": [
                {
                    "direction": direction,
                    "length": len(frame),
                    "prefix_hex": self._frame_prefix(frame, limit=96),
                }
                for direction, frame in self.frame_trace
            ],
            "error": self._error_msg or None,
        }

    @staticmethod
    def _frame_prefix(frame: Optional[bytes], limit: int = 64) -> Optional[str]:
        if frame is None:
            return None
        return frame[:limit].hex(" ")


# ══════════════════════════════════════════════════════════════════════
#  Slirp — user-mode networking through libslirp
# ══════════════════════════════════════════════════════════════════════

# The slirp network matches the TAP layout above, so a guest configured for
# either finds its gateway: the gateway is 10.64.0.1, DHCP hands the guest
# 10.64.0.2, and libslirp's DNS forwarder, which asks the host's own
# resolver, answers at 10.64.0.3.
SLIRP_NETWORK = "10.64.0.0"
SLIRP_PREFIX = 24
SLIRP_GATEWAY = "10.64.0.1"
SLIRP_GUEST = "10.64.0.2"
SLIRP_NAMESERVER = "10.64.0.3"

LOCAL_ACCESS_WARNING = (
    "Local access is on: any program on this machine, an installed applet or "
    "code that a feed or page leads it to run included, can reach services "
    "on this computer and its local network that expect only local callers."
)


class SlirpBackend(NICBackend):
    """The machine's user-mode network, through libslirp.

    libslirp turns the guest's Ethernet frames into ordinary connections
    from the user's own account, with DHCP and a DNS forwarder of its own,
    so the machine reaches the internet without administrator rights.  It
    runs inside MegaPad's _mp64_slirp module, on a thread of its own, and
    this backend hands each frame for the guest to on_rx_frame from a
    thread of its own as the TAP backend does.

    Unless LOCAL_ACCESS is true, the guest's packets to this computer and to
    local addresses are refused (shared/accel/slirp_network.cpp).  The
    backend keeps counts and no frames, since frames show what the user
    does.
    """

    def __init__(self, *, local_access: bool = False):
        super().__init__()
        self._local_access = bool(local_access)
        self._network = None
        self._thread: Optional[threading.Thread] = None
        self._running = False

    @property
    def local_access(self) -> bool:
        return self._local_access

    def send(self, frame: bytes) -> bool:
        network = self._network
        if network is None:
            return False
        return network.send(bytes(frame))

    def start(self):
        import _mp64_slirp

        self._network = _mp64_slirp.SlirpNetwork(
            SLIRP_NETWORK, SLIRP_PREFIX, SLIRP_GATEWAY, SLIRP_GUEST,
            SLIRP_NAMESERVER, self._local_access)
        self._running = True
        self._thread = threading.Thread(
            target=self._rx_loop, daemon=True, name="NIC-slirp")
        self._thread.start()

    def stop(self):
        self._running = False
        if self._thread is not None:
            self._thread.join(timeout=2.0)
            self._thread = None
        if self._network is not None:
            self._network.close()
            self._network = None

    def _rx_loop(self):
        network = self._network
        while self._running:
            frame = network.receive(0.25)
            if frame is not None and self.on_rx_frame is not None:
                self.on_rx_frame(frame)

    @property
    def link_up(self) -> bool:
        return self._network is not None

    @property
    def name(self) -> str:
        return "slirp, local access" if self._local_access else "slirp"

    def stats(self) -> dict:
        if self._network is None:
            return {"backend": "slirp", "link_up": False,
                    "local_access": self._local_access}
        return {"backend": "slirp", "link_up": True, **self._network.stats()}


def backend_from_spec(spec: str, *, local_access: bool = False) -> NICBackend:
    """The backend a server's --nic SPEC names: "slirp", or "tap:IFNAME" for
    a TAP device set up beforehand, which must exist and be open to this
    account.  LOCAL_ACCESS applies to slirp alone, since a TAP device's reach
    is the host's own routing."""

    if spec == "slirp":
        return SlirpBackend(local_access=local_access)
    if local_access:
        raise ValueError("local access applies to the slirp network alone")
    if spec.startswith("tap:") and len(spec) > 4:
        name = spec[4:]
        if not tap_available(name):
            raise ValueError(f"TAP device {name!r} does not exist or is not accessible")
        return TAPBackend(tap_name=name)
    raise ValueError(f"unknown network {spec!r}: use slirp or tap:IFNAME")


# ══════════════════════════════════════════════════════════════════════
#  Helpers
# ══════════════════════════════════════════════════════════════════════

def tap_available(tap_name: str = "mp64tap0") -> bool:
    """Test whether the named TAP device can be opened.

    Returns True if /dev/net/tun exists and the TAP interface is
    accessible by the current user.  Does not leave any fd open.

    Uses a retry loop because TUNSETIFF returns EBUSY when another
    process already holds the TAP fd (e.g. concurrent pytest-xdist
    workers all probing at import time).
    """
    if not os.path.exists("/dev/net/tun"):
        return False
    for _ in range(5):
        try:
            fd = TAPBackend._open_tap(tap_name)
            os.close(fd)
            return True
        except OSError:
            time.sleep(0.1)
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
