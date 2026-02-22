"""
NIC Backend Unit Tests for Megapad-64
=====================================
Unit tests for the NIC backend abstraction layer (Loopback, UDP, TAP)
and backward-compatible NetworkDevice behavior.

Live-network (TAP) integration tests have moved to ``test_live_net.py``.
"""

from __future__ import annotations

import time
import unittest

from nic_backends import TAPBackend, tap_available

EMU_MAC = b'\x02\x4D\x50\x36\x34\x00'


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
#  Backward compatibility
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
