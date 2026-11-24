"""KDOS on the simulator reaches the internet through the slirp network,
without administrator rights.

These tests reach real servers, so they run only on request:

    MP64_LIVE_NET=1 make test-sequential TEST_PATH=tests/test_live_net_slirp.py

They resolve example.com through libslirp's DNS forwarder, which asks this
computer's own resolver, and fetch its page's headers over plain HTTP.  The
check that the guest's words load runs every time, since it reaches no
server.
"""

from __future__ import annotations

import os

import pytest

from tests.test_network_slirp import _cell, _network_machine, _run_guest

live = pytest.mark.skipif(
    os.environ.get("MP64_LIVE_NET") != "1",
    reason="reaches real servers; set MP64_LIVE_NET=1 to run",
)

HOST = b"example.com"
REQUEST = b"HEAD / HTTP/1.0\r\nHost: example.com\r\nConnection: close\r\n\r\n"


def _bytes(name: str, data: bytes) -> bytes:
    """A CREATE of NAME holding DATA, sixteen bytes to a line, since each line
    of the boot script must fit MegaPad's 255-byte input buffer."""
    lines = [f"CREATE {name}".encode()]
    for start in range(0, len(data), 16):
        lines.append(b" ".join(b"%d C," % byte for byte in data[start:start + 16]))
    return b"\n".join(lines) + b"\n"


LIVE_WORDS = (
    _bytes("LT-HOST", HOST) + _bytes("LT-REQUEST", REQUEST) + b"""
CREATE LT-REPLY 256 ALLOT
VARIABLE LT-DHCP
VARIABLE LT-IP
VARIABLE LT-TCB
VARIABLE LT-STATE
VARIABLE LT-LEN
VARIABLE LT-DONE
: LT-ROOT  ( -- )
    DHCP-START LT-DHCP !
    LT-HOST """ + str(len(HOST)).encode() + b""" DNS-RESOLVE DUP LT-IP !
    IF
        LT-IP @ 80 0 TCP-CONNECT DUP LT-TCB !
        IF
            300 0 DO
                TCP-POLL NET-IDLE
                LT-TCB @ TCB.STATE @ TCPS-SYN-SENT <> IF LEAVE THEN
            LOOP
            LT-TCB @ TCB.STATE @ LT-STATE !
            LT-STATE @ TCPS-ESTABLISHED = IF
                LT-TCB @ LT-REQUEST """ + str(len(REQUEST)).encode() + b""" TCP-SEND DROP
                300 0 DO
                    TCP-POLL NET-IDLE
                    LT-TCB @ LT-REPLY 256 TCP-RECV DUP IF LT-LEN ! LEAVE THEN DROP
                LOOP
            THEN
        THEN
    THEN
    -1 LT-DONE ! ;
""")


def test_the_live_words_load_in_the_boot_script(tmp_path):
    assert max(len(line) for line in LIVE_WORDS.splitlines()) <= 255
    runtime, card = _network_machine(tmp_path, local_access=False, words=LIVE_WORDS)
    try:
        assert runtime.find("LT-ROOT") is not None
        request = runtime.find("LT-REQUEST").body_address
        assert runtime.memory.read_bytes(request, len(REQUEST)) == REQUEST
    finally:
        card.stop()


@live
def test_kdos_resolves_a_name_and_fetches_a_page_from_the_internet(tmp_path):
    runtime, card = _network_machine(tmp_path, local_access=False, words=LIVE_WORDS)
    try:
        _run_guest(runtime, "LT-ROOT", "LT-DONE", seconds=60)
        assert _cell(runtime, "LT-DHCP") != 0
        assert _cell(runtime, "LT-IP") != 0, "example.com did not resolve"
        assert _cell(runtime, "LT-STATE") == 4, "no connection to example.com"
        length = _cell(runtime, "LT-LEN")
        reply = runtime.memory.read_bytes(runtime.find("LT-REPLY").body_address, length)
        assert reply.startswith(b"HTTP/1."), reply[:40]
    finally:
        card.stop()
