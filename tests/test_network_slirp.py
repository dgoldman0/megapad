"""The user-mode network through libslirp (shared/accel/slirp_network.cpp,
nic_backends.SlirpBackend), first frame by frame and then under KDOS's own
network stack on the simulator.

Nothing here reaches the internet: the services these tests reach are
libslirp's own DHCP server and gateway, and listeners on this host, which
the network refuses unless local access is on.
"""

from __future__ import annotations

import socket
import struct
import threading
import time
from pathlib import Path

import pytest

import _mp64_slirp
from diskutil import FTYPE_FORTH, format_image, inject_file, pack_forth_source
from nic_backends import (
    LOCAL_ACCESS_WARNING,
    SLIRP_GATEWAY,
    SLIRP_GUEST,
    SLIRP_NAMESERVER,
    SlirpBackend,
    backend_from_spec,
)
from simulator.image_bootstrap import prepare_image_bootstrap
from simulator.network import NetworkCard
from simulator.platform import Topology, create_address_space
from simulator.runtime import MegaForthRuntime
from simulator.session import SimulatorMachineSession, SimulatorSharedMachine
from simulator.storage import HostedStorageService

REPOSITORY = Path(__file__).resolve().parents[1]
GUEST_MAC = bytes.fromhex("024d50363400")
BROADCAST = b"\xff" * 6
TCP_SYN, TCP_ACK = 0x02, 0x10


# Frames, as the guest would send them.

def _checksum(data: bytes) -> int:
    if len(data) % 2:
        data += b"\0"
    total = sum(struct.unpack("!%dH" % (len(data) // 2), data))
    while total >> 16:
        total = (total & 0xFFFF) + (total >> 16)
    return ~total & 0xFFFF


def _ethernet(destination: bytes, payload: bytes, kind: int = 0x0800) -> bytes:
    return destination + GUEST_MAC + struct.pack("!H", kind) + payload


def _ipv4(source: str, destination: str, protocol: int, payload: bytes) -> bytes:
    header = struct.pack("!BBHHHBBH4s4s", 0x45, 0, 20 + len(payload), 1, 0, 64, protocol,
                         0, socket.inet_aton(source), socket.inet_aton(destination))
    return header[:10] + struct.pack("!H", _checksum(header)) + header[12:] + payload


def _udp(source_port: int, destination_port: int, data: bytes) -> bytes:
    return struct.pack("!HHHH", source_port, destination_port, 8 + len(data), 0) + data


def _syn(source: str, destination: str, port: int) -> bytes:
    header = struct.pack("!HHIIBBHHH", 40000, port, 1000, 0, 5 << 4, TCP_SYN, 65535, 0, 0)
    pseudo = (socket.inet_aton(source) + socket.inet_aton(destination)
              + struct.pack("!BBH", 0, 6, len(header)))
    return header[:16] + struct.pack("!H", _checksum(pseudo + header)) + header[18:]


def _dhcp_discover() -> bytes:
    body = struct.pack("!BBBBIHH4s4s4s4s16s64s128s", 1, 1, 6, 0, 0x1234ABCD, 0, 0x8000,
                       bytes(4), bytes(4), bytes(4), bytes(4), GUEST_MAC + bytes(10),
                       bytes(64), bytes(128))
    body += bytes([99, 130, 83, 99, 53, 1, 1, 255])
    return _ethernet(BROADCAST, _ipv4("0.0.0.0", "255.255.255.255", 17, _udp(68, 67, body)))


def _arp_request(target: str) -> bytes:
    body = struct.pack("!HHBBH6s4s6s4s", 1, 0x0800, 6, 4, 1, GUEST_MAC,
                       socket.inet_aton(SLIRP_GUEST), bytes(6), socket.inet_aton(target))
    return _ethernet(BROADCAST, body, 0x0806)


def _syn_frame(gateway_mac: bytes, destination: str, port: int) -> bytes:
    return _ethernet(gateway_mac, _ipv4(SLIRP_GUEST, destination, 6,
                                        _syn(SLIRP_GUEST, destination, port)))


def _frames(network, seconds: float) -> list[bytes]:
    frames = []
    deadline = time.monotonic() + seconds
    while (left := deadline - time.monotonic()) > 0:
        frame = network.receive(left)
        if frame is not None:
            frames.append(frame)
    return frames


def _dhcp_options(frame: bytes) -> dict[int, bytes]:
    options, at = {}, 14 + 20 + 8 + 240
    while at < len(frame) and frame[at] != 255:
        if frame[at] == 0:
            at += 1
            continue
        options[frame[at]] = frame[at + 2:at + 2 + frame[at + 1]]
        at += 2 + frame[at + 1]
    return options


def _tcp_flags(frame: bytes) -> int | None:
    if frame[12:14] != b"\x08\x00" or frame[23] != 6:
        return None
    return frame[14 + 20 + 13]


@pytest.fixture
def network():
    opened = []

    def open_network(*, local_access: bool = False):
        network = _mp64_slirp.SlirpNetwork("10.64.0.0", 24, SLIRP_GATEWAY, SLIRP_GUEST,
                                           SLIRP_NAMESERVER, local_access)
        opened.append(network)
        return network

    yield open_network
    for network in opened:
        network.close()


def _gateway_mac(network) -> bytes:
    network.send(_arp_request(SLIRP_GATEWAY))
    replies = [f for f in _frames(network, 0.3) if f[12:14] == b"\x08\x06"]
    assert replies, "the gateway did not answer ARP"
    return replies[0][22:28]


@pytest.fixture
def listener():
    """A TCP service on this host's loopback, which records each caller."""

    server = socket.socket()
    server.bind(("127.0.0.1", 0))
    server.listen(4)
    server.settimeout(0.2)
    callers: list[tuple[str, bytes]] = []
    stop = threading.Event()

    def serve():
        while not stop.is_set():
            try:
                connection, address = server.accept()
            except OSError:
                continue
            connection.settimeout(2.0)
            try:
                data = connection.recv(64)
            except OSError:
                data = b""
            callers.append((address[0], data))
            connection.close()

    thread = threading.Thread(target=serve, daemon=True)
    thread.start()
    yield server.getsockname()[1], callers
    stop.set()
    thread.join()
    server.close()


# The network, frame by frame.

def test_dhcp_hands_the_guest_the_layout_kdos_expects(network):
    net = network()
    net.send(_dhcp_discover())
    offers = [f for f in _frames(net, 0.5) if f[12:14] == b"\x08\x00" and f[23] == 17]
    assert offers, "no DHCP offer"
    offer = offers[0]
    assert socket.inet_ntoa(offer[14 + 20 + 8 + 16:14 + 20 + 8 + 20]) == SLIRP_GUEST
    options = _dhcp_options(offer)
    assert socket.inet_ntoa(options[1]) == "255.255.255.0"
    assert socket.inet_ntoa(options[3][:4]) == SLIRP_GATEWAY
    assert socket.inet_ntoa(options[6][:4]) == SLIRP_NAMESERVER


@pytest.mark.parametrize("destination", [
    "192.168.1.1", "10.1.2.3", "172.16.0.1", "172.31.255.254", "169.254.10.10",
    "127.0.0.1", "100.64.0.1", "224.0.0.251", "0.0.0.1", "240.0.0.1",
])
def test_local_destinations_are_refused_without_local_access(network, destination):
    net = network()
    assert net.send(_syn_frame(_gateway_mac(net), destination, 80))
    assert net.stats()["refused"] == 1
    assert _mp64_slirp.local_destination(destination)


@pytest.mark.parametrize("destination", ["1.1.1.1", "93.184.216.34", "172.32.0.1"])
def test_internet_destinations_are_not_local(destination):
    assert not _mp64_slirp.local_destination(destination)


def test_ipv6_is_refused(network):
    net = network()
    assert net.send(_ethernet(BROADCAST, bytes(40), 0x86DD))
    assert net.stats()["refused"] == 1


def test_this_hosts_own_address_is_refused(network):
    probe = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    try:
        probe.connect(("192.0.2.1", 9))  # chooses a route; sends nothing
        own = probe.getsockname()[0]
    except OSError:
        pytest.skip("this host has no route off its loopback")
    finally:
        probe.close()
    net = network()
    net.send(_syn_frame(_gateway_mac(net), own, 80))
    assert net.stats()["refused"] == 1


def test_the_host_through_the_gateway_is_refused_without_local_access(network, listener):
    port, callers = listener
    net = network()
    net.send(_syn_frame(_gateway_mac(net), SLIRP_GATEWAY, port))
    answers = [_tcp_flags(f) for f in _frames(net, 0.5)]
    assert not any(flags is not None and flags & TCP_SYN for flags in answers)
    assert callers == []


def test_local_access_reaches_a_service_on_this_host(network, listener):
    port, callers = listener
    net = network(local_access=True)
    assert net.local_access
    net.send(_syn_frame(_gateway_mac(net), SLIRP_GATEWAY, port))
    answers = [_tcp_flags(f) for f in _frames(net, 0.5)]
    assert (TCP_SYN | TCP_ACK) in answers
    deadline = time.monotonic() + 2
    while not callers and time.monotonic() < deadline:
        time.sleep(0.01)
    assert callers and callers[0][0] == "127.0.0.1"


def test_frames_the_card_cannot_carry_are_rejected(network):
    net = network()
    assert not net.send(b"short")
    assert not net.send(bytes(_mp64_slirp.MAX_FRAME + 1))


def test_a_closed_network_carries_nothing(network):
    net = network()
    net.close()
    assert not net.send(_dhcp_discover())
    assert net.receive(0.01) is None


def test_settings_outside_the_network_are_refused():
    with pytest.raises(ValueError, match="must lie in the network"):
        _mp64_slirp.SlirpNetwork("10.64.0.0", 24, "10.65.0.1", SLIRP_GUEST,
                                 SLIRP_NAMESERVER, False)


def test_a_server_names_its_network():
    assert backend_from_spec("slirp").name == "slirp"
    assert backend_from_spec("slirp", local_access=True).name == "slirp, local access"
    assert backend_from_spec("tap:mp64tap0").name == "tap:mp64tap0"
    with pytest.raises(ValueError, match="slirp network alone"):
        backend_from_spec("tap:mp64tap0", local_access=True)
    with pytest.raises(ValueError, match="unknown network"):
        backend_from_spec("udp")
    assert "local network" in LOCAL_ACCESS_WARNING


# KDOS's own network stack on the simulator.

NETWORK_TEST_WORDS = b"""
CREATE NT-HOST 4 ALLOT  10 64 0 1 NT-HOST IP!
CREATE NT-HELLO  104 C, 101 C, 108 C, 108 C, 111 C,
VARIABLE NT-PORT
VARIABLE NT-DHCP
VARIABLE NT-TCB
VARIABLE NT-STATE
VARIABLE NT-DONE
: NT-ROOT  ( -- )
    DHCP-START NT-DHCP !
    NT-HOST NT-PORT @ 0 TCP-CONNECT DUP NT-TCB !
    IF
        150 0 DO
            TCP-POLL NET-IDLE
            NT-TCB @ TCB.STATE @ TCPS-SYN-SENT <> IF LEAVE THEN
        LOOP
        NT-TCB @ TCB.STATE @ NT-STATE !
        NT-STATE @ TCPS-ESTABLISHED = IF
            NT-TCB @ NT-HELLO 5 TCP-SEND DROP
            20 0 DO TCP-POLL NET-IDLE LOOP
        THEN
    ELSE
        -1 NT-STATE !
    THEN
    -1 NT-DONE ! ;
"""


def _network_machine(tmp_path: Path, *, local_access: bool,
                     words: bytes = NETWORK_TEST_WORDS):
    """KDOS with its network stack booted on the simulator over a slirp card,
    with WORDS defined after networking.f loads."""

    image = tmp_path / "network.img"
    format_image(str(image))
    inject_file(str(image), "kdos.f", (REPOSITORY / "kdos.f").read_bytes(), ftype=FTYPE_FORTH)
    inject_file(str(image), "networking.f",
                pack_forth_source((REPOSITORY / "networking.f").read_bytes()),
                ftype=FTYPE_FORTH)
    inject_file(str(image), "autoexec.f",
                b"ENTER-USERLAND\nREQUIRE networking.f\n" + words,
                ftype=FTYPE_FORTH)
    memory = create_address_space(
        topology=Topology(full_cores=1), bank0_size=1 << 20, external_size=128 << 20,
        vram_size=4 << 20, hbw_size=3 << 20, initial_epoch_ms=time.time_ns() // 1_000_000)
    memory.mmio.rtc.bind_monotonic_clock(time.monotonic_ns)
    storage = HostedStorageService(image_path=image, write_protected=False)
    runtime = MegaForthRuntime(memory=memory, storage=storage, execution_backend="native")
    card = NetworkCard(SlirpBackend(local_access=local_access))
    runtime.attach_network(card)
    card.start()
    prepare_image_bootstrap(memory=memory, storage=storage, runtime=runtime, session=False)
    return runtime, card


def _cell(runtime: MegaForthRuntime, name: str) -> int:
    return runtime.memory.read64(runtime.find(name).body_address)


def _run_guest(runtime: MegaForthRuntime, root: str, done: str, seconds: float = 30) -> None:
    """Run ROOT as the machine's session until it sets the variable DONE, as
    a server's machine owner runs it, so the guest's waits take real time."""

    machine = SimulatorSharedMachine(SimulatorMachineSession(runtime, root))
    machine.start()
    try:
        deadline = time.monotonic() + seconds
        while _cell(runtime, done) == 0 and time.monotonic() < deadline:
            time.sleep(0.02)
        assert machine.last_error is None
        assert _cell(runtime, done) != 0, "the guest did not finish"
    finally:
        machine.stop()


def _run_root(runtime: MegaForthRuntime, port: int) -> None:
    runtime.memory.write64(runtime.find("NT-PORT").body_address, port)
    _run_guest(runtime, "NT-ROOT", "NT-DONE")


def test_kdos_takes_its_address_by_dhcp_and_reaches_this_host_with_local_access(
        tmp_path, listener):
    port, callers = listener
    runtime, card = _network_machine(tmp_path, local_access=True)
    try:
        _run_root(runtime, port)
        assert _cell(runtime, "NT-DHCP") != 0
        for name, expected in (("MY-IP", SLIRP_GUEST), ("GW-IP", SLIRP_GATEWAY),
                               ("DNS-SERVER-IP", SLIRP_NAMESERVER)):
            address = runtime.find(name).body_address
            assert socket.inet_ntoa(runtime.memory.read_bytes(address, 4)) == expected
        assert _cell(runtime, "NT-STATE") == 4  # TCPS-ESTABLISHED
        deadline = time.monotonic() + 3
        while not callers and time.monotonic() < deadline:
            time.sleep(0.01)
        assert callers == [("127.0.0.1", b"hello")]
    finally:
        card.stop()


def test_kdos_cannot_reach_this_host_without_local_access(tmp_path, listener):
    port, callers = listener
    runtime, card = _network_machine(tmp_path, local_access=False)
    try:
        _run_root(runtime, port)
        assert _cell(runtime, "NT-DHCP") != 0
        assert _cell(runtime, "NT-STATE") != 4
        assert callers == []
    finally:
        card.stop()


# The simulator server's network options.

def _server_args(tmp_path: Path, *options: str):
    from simulator.server import build_argument_parser
    from tests.simulator.test_image_bootstrap import _boot_image

    image = tmp_path / "server.img"
    image.write_bytes(_boot_image())
    return build_argument_parser().parse_args(
        ["--storage", str(image), "--socket", str(tmp_path / "server.sock"),
         "--ext-mem-mib", "4", "--paused", *options])


def test_the_server_attaches_a_slirp_card_before_the_boot(tmp_path, capsys):
    from simulator.network import STATUS_LINK, STATUS_PRESENT
    from simulator.server import prepare_server, report_network

    prepared = prepare_server(_server_args(tmp_path, "--nic", "slirp"))
    try:
        runtime = prepared.preparation.runtime
        assert runtime.network is prepared.network
        assert prepared.network.status() == STATUS_PRESENT | STATUS_LINK
        report_network(prepared.network)
        captured = capsys.readouterr()
        assert "[shared] network: slirp" in captured.out
        assert captured.err == ""
    finally:
        prepared.network.stop()


def test_the_server_warns_at_every_start_with_local_access(tmp_path, capsys):
    from simulator.server import prepare_server, report_network

    prepared = prepare_server(_server_args(tmp_path, "--nic", "slirp", "--local-access"))
    try:
        report_network(prepared.network)
        captured = capsys.readouterr()
        assert "[shared] network: slirp, local access" in captured.out
        assert LOCAL_ACCESS_WARNING in captured.err
    finally:
        prepared.network.stop()


def test_the_server_refuses_local_access_without_slirp(tmp_path):
    from simulator.server import prepare_server

    with pytest.raises(ValueError, match="needs --nic slirp"):
        prepare_server(_server_args(tmp_path, "--local-access"))
    with pytest.raises(ValueError, match="slirp network alone"):
        prepare_server(_server_args(tmp_path, "--nic", "tap:mp64tap0", "--local-access"))


def test_a_server_without_a_card_has_no_network(tmp_path, capsys):
    from simulator.server import prepare_server, report_network

    prepared = prepare_server(_server_args(tmp_path))
    assert prepared.network is None
    assert prepared.preparation.runtime.network is None
    report_network(None)
    assert "[shared] network: none" in capsys.readouterr().out
