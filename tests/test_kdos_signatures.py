"""KDOS certificate and signature verification: DER, X.509, ECDSA P-256 and
P-384, RSA of every width through the multi-precision Montgomery module, the
trust store's path validation, and TLS CertificateVerify dispatch.

These classes run on the KDOS + networking snapshot that tests/test_system.py
builds, as their own unit, so a change to the certificate primitives can be
checked without the whole system test.
"""

import base64
import hashlib
import json
import os
import random
import re
import unittest
from pathlib import Path

from tests.test_system import (  # noqa: F401  (the harness the classes share)
    NETWORKING_PATH,
    PROJECT_ROOT,
    _KDOSNetworkTestBase,
    _networking_definition,
)


def mpta_record(cert: bytes, scope: bytes = b"", *, flags: int = 0,
                distrust_after: int = 0, constraints: bytes = b"") -> bytes:
    """One MPTA version 2 trust record."""
    return (
        flags.to_bytes(2, "big") + len(scope).to_bytes(2, "big") +
        len(constraints).to_bytes(2, "big") + len(cert).to_bytes(4, "big") +
        distrust_after.to_bytes(8, "big") + scope + constraints + cert
    )


def mpta_bundle(*records: bytes, generation: int = 1) -> bytes:
    """An MPTA version 2 bundle of the given records."""
    return (
        b"MPTA" + (2).to_bytes(2, "big") + len(records).to_bytes(2, "big") +
        generation.to_bytes(8, "big") + b"".join(records)
    )


# Drive a certificate message's path search to its end, as a poller would.
PARSE_CERT_WORD = (
    ": tv-parse-cert ( msg len -- ior ) TLS-PARSE-CERTIFICATE "
    "BEGIN DUP X509-PATH-PENDING = WHILE DROP TLS-CERTIFICATE-STEP REPEAT ;"
)


class TestKDOSASN1(_KDOSNetworkTestBase):
    """Tests for §16.7a ASN.1/DER minimal parser."""

    def _store_bytes(self, name, data: bytes) -> list[str]:
        """Generate Forth to CREATE a buffer and fill it with given bytes."""
        lines = [f"CREATE {name} {len(data)} ALLOT"]
        for i, b in enumerate(data):
            lines.append(f"{b} {name} {i} + C!")
        return lines

    def test_der_tag_read(self):
        """DER-TAG@ reads the tag byte."""
        lines = self._store_bytes("tv-buf", bytes([0x30, 0x03, 0x01, 0x01, 0xFF]))
        lines.append('." TAG=" tv-buf DER-TAG@ .')
        text = self._run_kdos(lines)
        self.assertIn("TAG=48 ", text)  # 0x30 = SEQUENCE

    def test_der_len_short(self):
        """DER-LEN@ decodes short form length."""
        lines = self._store_bytes("tv-buf", bytes([0x30, 0x03, 0x01, 0x01, 0xFF]))
        lines.append('." LEN=" tv-buf 1 + DER-LEN@ . ." HBYTES=" .')
        text = self._run_kdos(lines)
        self.assertIn("LEN=1 ", text)   # hdr-bytes = 1
        self.assertIn("HBYTES=3 ", text)  # length = 3

    def test_der_len_long_81(self):
        """DER-LEN@ decodes long form 81 xx."""
        # 0x81 0x80 = length 128
        lines = self._store_bytes("tv-buf", bytes([0x30, 0x81, 0x80]))
        lines.append('." LEN=" tv-buf 1 + DER-LEN@ . ." HBYTES=" .')
        text = self._run_kdos(lines)
        self.assertIn("LEN=2 ", text)   # hdr-bytes = 2
        self.assertIn("HBYTES=128 ", text)

    def test_der_next_simple(self):
        """DER-NEXT parses a simple TLV and returns correct addresses."""
        # SEQUENCE(3) { BOOLEAN TRUE }
        lines = self._store_bytes("tv-buf", bytes([0x30, 0x03, 0x01, 0x01, 0xFF]))
        lines.extend([
            "tv-buf DER-NEXT",
            '." NEXT=" tv-buf - .',       # next-addr offset
            '." VLEN=" .',                  # val-len
            '." VOFF=" tv-buf - .',        # val-addr offset
        ])
        text = self._run_kdos(lines)
        self.assertIn("VOFF=2 ", text)   # value starts at offset 2
        self.assertIn("VLEN=3 ", text)   # value length 3
        self.assertIn("NEXT=5 ", text)   # next element at offset 5

    def test_der_skip(self):
        """DER-SKIP advances past one TLV element."""
        # Two consecutive INTEGERs: INT(1)=0x42, INT(1)=0x17
        data = bytes([0x02, 0x01, 0x42, 0x02, 0x01, 0x17])
        lines = self._store_bytes("tv-buf", data)
        lines.extend([
            "tv-buf DER-SKIP",
            '." TAG2=" DUP C@ .',           # should be 0x02 (INTEGER)
            "DER-NEXT DROP",
            '." VAL2=" SWAP C@ .',          # should be 0x17
        ])
        text = self._run_kdos(lines)
        self.assertIn("TAG2=2 ", text)
        self.assertIn("VAL2=23 ", text)  # 0x17 = 23

    def test_der_find_tag(self):
        """DER-FIND-TAG finds a specific tag in a sequence of TLVs."""
        # SEQUENCE{ INT(42), BOOL(true), OCT_STRING("AB") }
        data = bytes([
            0x30, 0x09,                   # SEQUENCE, length 9
            0x02, 0x01, 0x2A,             # INTEGER 42
            0x01, 0x01, 0xFF,             # BOOLEAN TRUE
            0x04, 0x02, 0x41, 0x42,       # OCTET STRING "AB"
        ])
        lines = self._store_bytes("tv-buf", data)
        lines.extend([
            # Enter the SEQUENCE, then find OCTET STRING (tag 4)
            "tv-buf DER-ENTER DROP",       # inner-addr
            "9 4 DER-FIND-TAG",
            '." FLEN=" .',
            '." FB0=" DUP C@ .',
            '." FB1=" 1 + C@ .',
        ])
        text = self._run_kdos(lines)
        self.assertIn("FLEN=2 ", text)
        self.assertIn("FB0=65 ", text)   # 'A'
        self.assertIn("FB1=66 ", text)   # 'B'


# ---------------------------------------------------------------------------
#  X.509 Certificate Parser tests — §16.7b
# ---------------------------------------------------------------------------


class TestKDOSX509(_KDOSNetworkTestBase):
    """Tests for §16.7b X.509 leaf certificate parser."""

    # DER-encoded self-signed P-256 test certificate with SAN
    _CERT_DER = bytes([48, 130, 1, 102, 48, 130, 1, 12, 160, 3, 2, 1, 2, 2, 20,
        34, 27, 200, 225, 145, 138, 207, 34, 68, 176, 101, 180, 106, 85,
        246, 232, 230, 124, 74, 164, 48, 10, 6, 8, 42, 134, 72, 206, 61,
        4, 3, 2, 48, 27, 49, 25, 48, 23, 6, 3, 85, 4, 3, 12, 16, 116,
        101, 115, 116, 46, 101, 120, 97, 109, 112, 108, 101, 46, 99, 111,
        109, 48, 30, 23, 13, 50, 52, 48, 49, 48, 49, 48, 48, 48, 48, 48,
        48, 90, 23, 13, 50, 53, 49, 50, 51, 49, 48, 48, 48, 48, 48, 48,
        90, 48, 27, 49, 25, 48, 23, 6, 3, 85, 4, 3, 12, 16, 116, 101,
        115, 116, 46, 101, 120, 97, 109, 112, 108, 101, 46, 99, 111, 109,
        48, 89, 48, 19, 6, 7, 42, 134, 72, 206, 61, 2, 1, 6, 8, 42, 134,
        72, 206, 61, 3, 1, 7, 3, 66, 0, 4, 1, 253, 106, 192, 85, 127, 30,
        235, 113, 4, 237, 5, 178, 208, 51, 134, 176, 210, 233, 162, 14,
        74, 89, 33, 93, 197, 92, 125, 101, 231, 212, 248, 194, 104, 231,
        234, 137, 196, 21, 199, 62, 212, 208, 86, 234, 244, 27, 140, 199,
        120, 107, 238, 36, 249, 14, 44, 146, 16, 26, 236, 214, 33, 151,
        26, 163, 46, 48, 44, 48, 42, 6, 3, 85, 29, 17, 4, 35, 48, 33,
        130, 16, 116, 101, 115, 116, 46, 101, 120, 97, 109, 112, 108, 101,
        46, 99, 111, 109, 130, 13, 42, 46, 101, 120, 97, 109, 112, 108,
        101, 46, 99, 111, 109, 48, 10, 6, 8, 42, 134, 72, 206, 61, 4, 3,
        2, 3, 72, 0, 48, 69, 2, 32, 119, 35, 216, 78, 51, 29, 150, 190,
        215, 48, 178, 28, 238, 240, 193, 67, 65, 54, 224, 83, 95, 201,
        170, 106, 229, 86, 98, 142, 247, 227, 254, 108, 2, 33, 0, 159, 81,
        221, 61, 165, 60, 143, 129, 140, 175, 70, 103, 248, 76, 245, 239,
        32, 123, 229, 194, 87, 146, 233, 250, 49, 0, 84, 16, 173, 105,
        38, 134])

    # Expected public key bytes (uncompressed, 65 bytes)
    _PUB_KEY = bytes([4, 1, 253, 106, 192, 85, 127, 30, 235, 113, 4, 237, 5, 178,
        208, 51, 134, 176, 210, 233, 162, 14, 74, 89, 33, 93, 197, 92,
        125, 101, 231, 212, 248, 194, 104, 231, 234, 137, 196, 21, 199,
        62, 212, 208, 86, 234, 244, 27, 140, 199, 120, 107, 238, 36, 249,
        14, 44, 146, 16, 26, 236, 214, 33, 151, 26])

    def _store_bytes(self, name, data: bytes) -> list[str]:
        lines = [f"CREATE {name} {len(data)} ALLOT"]
        for i, b in enumerate(data):
            lines.append(f"{b} {name} {i} + C!")
        return lines

    def test_x509_parse_success(self):
        """X509-PARSE succeeds on valid P-256 certificate."""
        lines = self._store_bytes("tv-cert", self._CERT_DER)
        lines.extend([
            f"tv-cert {len(self._CERT_DER)} X509-PARSE",
            '." FLAG=" .',
            '." ALGO=" _X509-PUBKEY-ALGO @ .',
            '." PKLEN=" _X509-PUBKEY-LEN @ .',
        ])
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("FLAG=0 ", text)         # success
        self.assertIn("ALGO=2 ", text)         # X509-KEY-P256
        self.assertIn("PKLEN=65 ", text)       # uncompressed point

    def test_x509_parse_pubkey_bytes(self):
        """X509-PARSE extracts correct public key bytes."""
        lines = self._store_bytes("tv-cert", self._CERT_DER)
        lines.extend([
            f"tv-cert {len(self._CERT_DER)} X509-PARSE DROP",
            # Check first byte (0x04 = uncompressed)
            '." B0=" _X509-PUBKEY C@ .',
            # Check a few more key bytes
            '." B1=" _X509-PUBKEY 1 + C@ .',
            '." B2=" _X509-PUBKEY 2 + C@ .',
        ])
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn(f"B0={self._PUB_KEY[0]} ", text)  # 4
        self.assertIn(f"B1={self._PUB_KEY[1]} ", text)  # 1
        self.assertIn(f"B2={self._PUB_KEY[2]} ", text)  # 253

    def test_x509_parse_rejects_non_sequence(self):
        """X509-PARSE rejects a buffer that doesn't start with SEQUENCE."""
        lines = self._store_bytes("tv-bad", bytes([0x02, 0x01, 0x00]))
        lines.extend([
            "tv-bad 3 X509-PARSE",
            '." FLAG=" .',
        ])
        text = self._run_kdos(lines)
        # Should return -1 (which in unsigned 64-bit is a large number)
        # In Forth, -1 prints as -1 or as max unsigned depending on implementation
        self.assertTrue("FLAG=-1 " in text or "FLAG=18446744073709551615 " in text)

    def test_x509_check_host_exact(self):
        """X509-CHECK-HOST matches exact hostname from SAN."""
        lines = self._store_bytes("tv-cert", self._CERT_DER)
        lines.extend([
            f"tv-cert {len(self._CERT_DER)} X509-PARSE DROP",
            'S" test.example.com" X509-CHECK-HOST',
            '." MATCH=" .',
        ])
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("MATCH=0 ", text)  # 0 = matched

    def test_x509_check_host_wildcard(self):
        """X509-CHECK-HOST matches wildcard *.example.com."""
        lines = self._store_bytes("tv-cert", self._CERT_DER)
        lines.extend([
            f"tv-cert {len(self._CERT_DER)} X509-PARSE DROP",
            "VARIABLE wildcard-depth  DEPTH wildcard-depth !",
            'S" foo.example.com" X509-CHECK-HOST',
            '." MATCH=" .',
            'DEPTH wildcard-depth @ = ." BALANCED=" .',
        ])
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("MATCH=0 ", text)  # 0 = matched
        self.assertIn("BALANCED=-1 ", text)

    def test_x509_check_host_mismatch(self):
        """X509-CHECK-HOST rejects non-matching hostname."""
        lines = self._store_bytes("tv-cert", self._CERT_DER)
        lines.extend([
            f"tv-cert {len(self._CERT_DER)} X509-PARSE DROP",
            'S" evil.attacker.com" X509-CHECK-HOST',
            '." MATCH=" .',
        ])
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        # Should be -1 (no match)
        self.assertTrue("MATCH=-1 " in text or "MATCH=18446744073709551615 " in text)

    def test_x509_san_extracted(self):
        """X509-PARSE extracts SAN extension."""
        lines = self._store_bytes("tv-cert", self._CERT_DER)
        lines.extend([
            f"tv-cert {len(self._CERT_DER)} X509-PARSE DROP",
            '." SANLEN=" _X509-SAN-LEN @ .',
        ])
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        # SAN should be non-empty
        san_len = int(text.split("SANLEN=")[-1].split()[0])
        self.assertGreater(san_len, 0)


# ---------------------------------------------------------------------------
#  P-256 ECDSA Verification tests — §16.7c
# ---------------------------------------------------------------------------


class TestKDOSECDSA(_KDOSNetworkTestBase):
    """Tests for §16.7c P-256 ECDSA signature verification.
    Legacy public verification tests retain their checked-in large limits;
    private scalar and signer qualification use the ordinary 400M ceiling.
    """

    def _store_bytes(self, name, data: bytes) -> list[str]:
        lines = [f"CREATE {name} {len(data)} ALLOT"]
        for i, b in enumerate(data):
            lines.append(f"{b} {name} {i} + C!")
        return lines

    _forth_definition = staticmethod(_networking_definition)

    def _run_secret_scalar_case(
        self, scalar: bytes, expected_x: bytes, expected_y: bytes
    ) -> tuple[int, str]:
        """Run one scalar KAT from a fresh snapshot and return its cycle cost."""
        lines = self._store_bytes("pss-k", scalar)
        lines += self._store_bytes("pss-ex", expected_x)
        lines += self._store_bytes("pss-ey", expected_y)
        lines.extend([
            "CREATE pss-x 32 ALLOT",
            "CREATE pss-y 32 ALLOT",
            "VARIABLE pss-t0",
            "VARIABLE pss-delta",
            "VARIABLE pss-ior",
            "PERF-CYCLES pss-t0 !",
            "pss-k pss-x pss-y _P256-SECRET-BASE-MUL-CHECKED pss-ior !",
            "PERF-CYCLES pss-t0 @ - pss-delta !",
            '." IOR=" pss-ior @ .',
            '." CYC=" pss-delta @ .',
            "PRIME-P256",
            "pss-ex pss-x _EC-T1 FCEQ",
            '." X=" _EC-T1 C@ .',
            "pss-ey pss-y _EC-T1 FCEQ",
            '." Y=" _EC-T1 C@ .',
        ])
        text = self._run_kdos(lines)
        self.assertIn("IOR=0 ", text)
        self.assertIn("X=1 ", text)
        self.assertIn("Y=1 ", text)
        cycles = int(text.split("CYC=")[-1].split()[0])
        return cycles, text

    def test_p256_secret_schedule_is_fixed_and_separate(self):
        """Secret-scalar words have fixed loops and avoid legacy EC branches."""
        validate = self._forth_definition("_P256S-VALIDATE-SELECT")
        bit_load = self._forth_definition("_P256S-LOAD-BIT")
        double = self._forth_definition("_P256S-RCB-DOUBLE-A3")
        mixed = self._forth_definition("_P256S-RCB-MIXED-ADD-G-A3")
        select = self._forth_definition("_P256S-SELECT-R")
        core = self._forth_definition("_P256S-SECRET-G-MUL")
        wrapper = self._forth_definition("_P256-SECRET-BASE-MUL-CHECKED")
        cleanup = self._forth_definition("_P256S-FIELD-CLEAR")

        fixed_path = "\n".join((validate, bit_load, double, mixed, select, core))
        for forbidden in ("IF", "ELSE", "THEN", "EXIT", "FCMOV",
                          "EC-MUL", "EC-ADD", "EC-DOUBLE"):
            self.assertIsNone(
                re.search(rf"(?<![-A-Z0-9_]){re.escape(forbidden)}(?![-A-Z0-9_])",
                          fixed_path),
                f"secret scalar schedule must not use {forbidden}",
            )
        self.assertIn("32 0 DO", validate)
        self.assertIn("256 0 DO", core)
        self.assertEqual(core.count("_P256S-RCB-DOUBLE-A3"), 1)
        self.assertEqual(core.count("_P256S-RCB-MIXED-ADD-G-A3"), 1)
        self.assertEqual(core.count("_P256S-SELECT-R"), 1)
        self.assertEqual(len(re.findall(r"\bFSQR\b", double)), 3)
        self.assertEqual(len(re.findall(r"\bFMUL\b", double)), 10)
        self.assertEqual(len(re.findall(r"\b(?:FADD|FSUB)\b", double)), 21)
        self.assertEqual(len(re.findall(r"\bFMUL\b", mixed)), 13)
        self.assertEqual(len(re.findall(r"\b(?:FADD|FSUB)\b", mixed)), 23)
        self.assertEqual(len(re.findall(r"\bFMUL\b", select)), 3)
        self.assertEqual(len(re.findall(r"\bFADD\b", select)), 3)
        self.assertEqual(len(re.findall(r"\bFSUB\b", select)), 3)
        self.assertIn("CATCH", wrapper)
        self.assertIn("_P256S-UNWIND", wrapper)
        self.assertLess(cleanup.index("FINV"), cleanup.index("FMUL-RAW"))

    def test_p256_complete_formula_vectors(self):
        """Complete homogeneous formulas cover doubling and exceptional add."""
        two_g_x = bytes.fromhex(
            "7cf27b188d034f7e8a52380304b51ac3"
            "c08969e277f21b35a60b48fc47669978"
        )[::-1]
        two_g_y = bytes.fromhex(
            "07775510db8ed040293d9ac69f7430db"
            "ba7dade63ce982299e04b79d227873d1"
        )[::-1]
        neg_g_y = bytes.fromhex(
            "b01cbd1c01e58065711814b583f061e9"
            "d431cca994cea1313449bf97c840ae0a"
        )[::-1]
        lines = self._store_bytes("pss-2gx", two_g_x)
        lines += self._store_bytes("pss-2gy", two_g_y)
        lines += self._store_bytes("pss-ngy", neg_g_y)
        lines.extend([
            "_P256S-SETUP PRIME-P256",
            "P256-GX _P256S-RX 32 MOVE",
            "P256-GY _P256S-RY 32 MOVE",
            "_P256S-ONE _P256S-RZ 32 MOVE",
            "_P256S-RCB-DOUBLE-A3",
            "pss-2gx _P256S-DZ _P256S-T4 FMUL",
            "_P256S-DX _P256S-T4 _P256S-SEL-T FCEQ",
            '." DBLX=" _P256S-SEL-T C@ .',
            "pss-2gy _P256S-DZ _P256S-T4 FMUL",
            "_P256S-DY _P256S-T4 _P256S-SEL-T FCEQ",
            '." DBLY=" _P256S-SEL-T C@ .',
            "P256-GX _P256S-DX 32 MOVE",
            "P256-GY _P256S-DY 32 MOVE",
            "_P256S-ONE _P256S-DZ 32 MOVE",
            "_P256S-RCB-MIXED-ADD-G-A3",
            "pss-2gx _P256S-AZ _P256S-T4 FMUL",
            "_P256S-AX _P256S-T4 _P256S-SEL-T FCEQ",
            '." ADDX=" _P256S-SEL-T C@ .',
            "pss-2gy _P256S-AZ _P256S-T4 FMUL",
            "_P256S-AY _P256S-T4 _P256S-SEL-T FCEQ",
            '." ADDY=" _P256S-SEL-T C@ .',
            "P256-GX _P256S-DX 32 MOVE",
            "pss-ngy _P256S-DY 32 MOVE",
            "_P256S-ONE _P256S-DZ 32 MOVE",
            "_P256S-RCB-MIXED-ADD-G-A3",
            "_P256S-AZ _P256S-ZERO _P256S-SEL-T FCEQ",
            '." INVZ=" _P256S-SEL-T C@ .',
            "_P256S-ZERO _P256S-RX 32 MOVE",
            "_P256S-ONE _P256S-RY 32 MOVE",
            "_P256S-ZERO _P256S-RZ 32 MOVE",
            "_P256S-RCB-DOUBLE-A3",
            "_P256S-DX _P256S-ZERO _P256S-SEL-T FCEQ",
            '." OX=" _P256S-SEL-T C@ .',
            "_P256S-DY _P256S-ONE _P256S-SEL-T FCEQ",
            '." OY=" _P256S-SEL-T C@ .',
            "_P256S-DZ _P256S-ZERO _P256S-SEL-T FCEQ",
            '." OZ=" _P256S-SEL-T C@ .',
            "_P256S-CLEANUP",
        ])
        text = self._run_kdos(lines)
        for marker in ("DBLX", "DBLY", "ADDX", "ADDY", "INVZ",
                       "OX", "OY", "OZ"):
            self.assertIn(f"{marker}=1 ", text)

    def test_p256_secret_scalar_vectors_have_equal_cycles(self):
        """Different valid private scalars have the same architectural cost."""
        generator_x = bytes.fromhex(
            "6b17d1f2e12c4247f8bce6e563a440f2"
            "77037d812deb33a0f4a13945d898c296"
        )[::-1]
        generator_y = bytes.fromhex(
            "4fe342e2fe1a7f9b8ee7eb4a7c0f9e1"
            "62bce33576b315ececbb6406837bf51f5"
        )[::-1]
        rfc_private = bytes.fromhex(
            "c9afa9d845ba75166b5c215767b1d693"
            "4e50c3db36e89b127b8a622b120f6721"
        )[::-1]
        rfc_public_x = bytes.fromhex(
            "60fed4ba255a9d31c961eb74c6356d68"
            "c049b8923b61fa6ce669622e60f29fb6"
        )[::-1]
        rfc_public_y = bytes.fromhex(
            "7903fe1008b8bc99a41ae9e95628bc64"
            "f2f1b20c2d7e9f5177a3c294d4462299"
        )[::-1]
        scalar_one = bytes([1]) + bytes(31)

        one_cycles, _ = self._run_secret_scalar_case(
            scalar_one, generator_x, generator_y
        )
        rfc_cycles, _ = self._run_secret_scalar_case(
            rfc_private, rfc_public_x, rfc_public_y
        )
        self.assertEqual(one_cycles, rfc_cycles)

    def test_p256_secret_failures_are_atomic_and_scrubbed(self):
        """Invalid and aliased calls preserve outputs and clear owned state."""
        scalar_one = bytes([1]) + bytes(31)
        lines = self._store_bytes("pss-one", scalar_one)
        lines.extend([
            "CREATE pss-zero 32 ALLOT",
            "CREATE pss-sentinel 32 ALLOT",
            "CREATE pss-x 32 ALLOT",
            "CREATE pss-y 32 ALLOT",
            "CREATE pss-acc 32 ALLOT",
            "CREATE pss-lo 32 ALLOT",
            "CREATE pss-hi 32 ALLOT",
            "CREATE pss-gx-copy 32 ALLOT",
            "pss-zero 32 0 FILL",
            "pss-sentinel 32 165 FILL",
            "P256-GX pss-gx-copy 32 MOVE",
            "11 _TRO-A ! 12 _TRO-A-U ! 13 _TRO-B ! 14 _TRO-B-U !",
            ": pss-sentinel? ( a -- flag ) pss-sentinel _EC-T1 FCEQ _EC-T1 C@ 0<> ;",
            ": pss-work-zero? ( -- flag ) TRUE /P256-SECRET-WORK 0 DO",
            "  _P256-SECRET-WORK I + C@ 0= AND LOOP ;",
            "pss-sentinel pss-x 32 MOVE pss-sentinel pss-y 32 MOVE",
            "_P256-SECRET-WORK /P256-SECRET-WORK 90 FILL",
            "pss-zero pss-x pss-y _P256-SECRET-BASE-MUL-CHECKED",
            '." ZERO=" .',
            '." ZATOMIC=" pss-x pss-sentinel? pss-y pss-sentinel? AND .',
            "pss-sentinel pss-x 32 MOVE pss-sentinel pss-y 32 MOVE",
            "P256-N pss-x pss-y _P256-SECRET-BASE-MUL-CHECKED",
            '." N=" .',
            '." NATOMIC=" pss-x pss-sentinel? pss-y pss-sentinel? AND .',
            "pss-one pss-one pss-y _P256-SECRET-BASE-MUL-CHECKED",
            '." IOALIAS=" .',
            "pss-one pss-x pss-x _P256-SECRET-BASE-MUL-CHECKED",
            '." OOALIAS=" .',
            "pss-one _P256-SECRET-WORK pss-y _P256-SECRET-BASE-MUL-CHECKED",
            '." WSALIAS=" .',
            "TLS-OWNER-CORE pss-x pss-y _P256-SECRET-BASE-MUL-CHECKED",
            '." OWNERIN=" .',
            "pss-one TLS-OWNER-CORE pss-y _P256-SECRET-BASE-MUL-CHECKED",
            '." OWNEROUT=" .',
            "pss-one P256-GX pss-y _P256-SECRET-BASE-MUL-CHECKED",
            '." STATICOUT=" .',
            '." WORKZERO=" pss-work-zero? .',
            '." OWNER=" TLS-OWNER-DEPTH @ .',
            '." TRO=" _TRO-A @ 11 = _TRO-A-U @ 12 = AND _TRO-B @ 13 = AND _TRO-B-U @ 14 = AND .',
            "pss-acc 32 90 FILL pss-acc GF-R@",
            "pss-lo 32 90 FILL pss-hi 32 90 FILL",
            "pss-zero pss-zero pss-lo pss-hi FMUL-ADD-RAW",
            "P256-GX pss-gx-copy _EC-T1 FCEQ",
            '." STATICOK=" _EC-T1 C@ .',
            "pss-zero pss-acc _EC-T1 FCEQ",
            '." ACC=" _EC-T1 C@ .',
            "pss-zero pss-lo _EC-T1 FCEQ",
            '." PREVLO=" _EC-T1 C@ .',
            "pss-zero pss-hi _EC-T1 FCEQ",
            '." PREVHI=" _EC-T1 C@ .',
        ])
        text = self._run_kdos(lines)
        self.assertIn("ZERO=-4303 ", text)
        self.assertIn("N=-4303 ", text)
        self.assertIn("IOALIAS=-4302 ", text)
        self.assertIn("OOALIAS=-4302 ", text)
        self.assertIn("WSALIAS=-4302 ", text)
        self.assertIn("OWNERIN=-4302 ", text)
        self.assertIn("OWNEROUT=-4302 ", text)
        self.assertIn("STATICOUT=-4302 ", text)
        for marker in ("ZATOMIC", "NATOMIC", "WORKZERO", "TRO"):
            self.assertIn(f"{marker}=-1 ", text)
        for marker in ("STATICOK", "ACC", "PREVLO", "PREVHI"):
            self.assertIn(f"{marker}=1 ", text)
        self.assertIn("OWNER=0 ", text)

    def test_p256_secret_busy_is_non_mutating(self):
        """A foreign TLS owner gets busy without signer scratch mutation."""
        lines = self._store_bytes("pss-one", bytes([1]) + bytes(31))
        lines.extend([
            "CREATE pss-sentinel 32 ALLOT",
            "CREATE pss-x 32 ALLOT",
            "CREATE pss-y 32 ALLOT",
            "pss-sentinel 32 165 FILL",
            "pss-sentinel pss-x 32 MOVE pss-sentinel pss-y 32 MOVE",
            ": pss-sentinel? ( a -- flag ) pss-sentinel _EC-T1 FCEQ _EC-T1 C@ 0<> ;",
            ": pss-work-poison? ( -- flag ) TRUE /P256-SECRET-WORK 0 DO",
            "  _P256-SECRET-WORK I + C@ 90 = AND LOOP ;",
            "TLS-OWNER-TRY DROP",
            "TASK-ID 1+ TLS-OWNER-TASK !",
            "_P256-SECRET-WORK /P256-SECRET-WORK 90 FILL",
            "pss-one pss-x pss-y _P256-SECRET-BASE-MUL-CHECKED",
            '." BUSY=" .',
            '." ATOMIC=" pss-x pss-sentinel? pss-y pss-sentinel? AND .',
            '." WORK=" pss-work-poison? .',
            '." HELD=" TLS-OWNER-DEPTH @ .',
            "TASK-ID TLS-OWNER-TASK ! TLS-OWNER-RELEASE",
            '." RELEASED=" TLS-OWNER-DEPTH @ .',
        ])
        text = self._run_kdos(lines)
        self.assertIn("BUSY=-4206 ", text)
        self.assertIn("ATOMIC=-1 ", text)
        self.assertIn("WORK=-1 ", text)
        self.assertIn("HELD=1 ", text)
        self.assertIn("RELEASED=0 ", text)

    def test_p256_secret_range_and_recursive_owner_paths(self):
        """Range failures clean up, while nested ownership loses one depth."""
        lines = self._store_bytes("pss-one", bytes([1]) + bytes(31))
        lines.extend([
            "CREATE pss-x 32 ALLOT CREATE pss-y 32 ALLOT",
            "TLS-OWNER-TRY DROP",
            '0 pss-x pss-y _P256-SECRET-BASE-MUL-CHECKED ." RANGE=" .',
            '." D1=" TLS-OWNER-DEPTH @ .',
            "pss-one pss-x pss-y _P256-SECRET-BASE-MUL-CHECKED",
            '." OK=" .',
            '." D2=" TLS-OWNER-DEPTH @ .',
            "TLS-OWNER-RELEASE",
            '." D3=" TLS-OWNER-DEPTH @ .',
        ])
        text = self._run_kdos(lines)
        self.assertIn("RANGE=-4301 ", text)
        self.assertIn("D1=1 ", text)
        self.assertIn("OK=0 ", text)
        self.assertIn("D2=1 ", text)
        self.assertIn("D3=0 ", text)

    def test_p256_secret_unwind_rethrows_after_cleanup(self):
        """The signer finally path scrubs and releases before exact rethrow."""
        lines = [
            ": pss-work-zero? ( -- flag ) TRUE /P256-SECRET-WORK 0 DO",
            "  _P256-SECRET-WORK I + C@ 0= AND LOOP ;",
            ": pss-unwind-probe ( -- ) -4377 _P256S-UNWIND ;",
            "TLS-OWNER-TRY DROP",
            "_P256-SECRET-WORK /P256-SECRET-WORK 90 FILL",
            "' pss-unwind-probe CATCH",
            '." THROW=" .',
            '." WORKZERO=" pss-work-zero? .',
            '." OWNER=" TLS-OWNER-DEPTH @ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("THROW=-4377 ", text)
        self.assertIn("WORKZERO=-1 ", text)
        self.assertIn("OWNER=0 ", text)

    def test_rfc6979_state_matches_p256_sha256_candidates(self):
        """RFC 6979 state advances through and beyond one four-trial batch."""
        private_le = bytes.fromhex(
            "c9afa9d845ba75166b5c215767b1d693"
            "4e50c3db36e89b127b8a622b120f6721"
        )[::-1]
        digest = bytes.fromhex(
            "af2bdbe1aa9b6ec1e2ade1d694f41fc7"
            "1a831d0268e9891562113d8a62add1bf"
        )
        candidates = [bytes.fromhex(value) for value in (
            "a6e3c57dd01abe90086538398355dd4c"
            "3b17aa873382b0f24d6129493d8aad60",
            "8e83dc490bc5fc4d5992bd63cd87f254"
            "adffcb930f8a8011702a88870f638fdb",
            "7b8dc9ad8ce159abca1b9915fc1470e9"
            "1d5ad2443b3032557e78f47e180ab702",
            "0c2161730a70227da55c7d16df1b6e13"
            "02e751bab0caf723ff830f7ba60a30ad",
            "debb86c7cfe67ade22e77b9f14285f81"
            "9c64d8b154f5ac778731b91e785caf7c",
        )]
        lines = self._store_bytes("eps-d", private_le)
        lines += self._store_bytes("eps-h", digest)
        for index, candidate in enumerate(candidates, 1):
            lines += self._store_bytes(f"eps-k{index}", candidate)
        lines.extend([
            "CREATE eps-out 72 ALLOT",
            "TLS-OWNER-TRY DROP",
            "eps-d eps-h eps-out 72 _EPS-ARGS-STATUS DROP",
            "_EPS-SETUP _EPS-PREPROCESS _EPS-RFC-INIT",
        ])
        for index in range(1, 6):
            if index > 1:
                lines.append("_EPS-RFC-REJECT")
            lines.extend([
                "_EPS-RFC-CANDIDATE",
                f'." K{index}=" _EPS-V eps-k{index} 32 _XC-BYTES= .',
            ])
        lines.extend([
            "_EPS-CLEANUP TLS-OWNER-RELEASE",
            '." OWNER=" TLS-OWNER-DEPTH @ .',
        ])
        text = self._run_kdos(lines)
        for index in range(1, 6):
            self.assertIn(f"K{index}=-1 ", text)
        self.assertIn("OWNER=0 ", text)

    def test_rfc6979_bits2octets_reduces_one_full_width_digest(self):
        """bits2octets performs the required P-256 order reduction."""
        private_le = bytes.fromhex(
            "c9afa9d845ba75166b5c215767b1d693"
            "4e50c3db36e89b127b8a622b120f6721"
        )[::-1]
        reduced_le = bytes.fromhex(
            "00000000ffffffff0000000000000000"
            "4319055258e8617b0c46353d039cdaae"
        )[::-1]
        lines = self._store_bytes("eps-d", private_le)
        lines += self._store_bytes("eps-h", bytes([0xFF]) * 32)
        lines += self._store_bytes("eps-reduced", reduced_le)
        lines.extend([
            "CREATE eps-out 72 ALLOT",
            "TLS-OWNER-TRY DROP",
            "eps-d eps-h eps-out 72 _EPS-ARGS-STATUS DROP",
            "_EPS-SETUP _EPS-PREPROCESS",
            '." REDUCED=" _EPS-H1 eps-reduced 32 _XC-BYTES= .',
            "_EPS-CLEANUP TLS-OWNER-RELEASE",
            '." OWNER=" TLS-OWNER-DEPTH @ .',
        ])
        text = self._run_kdos(lines)
        self.assertIn("REDUCED=-1 ", text)
        self.assertIn("OWNER=0 ", text)

    def test_rfc6979_signer_has_four_complete_unbounded_trials(self):
        """The ordinary batch is fixed work; four is not a retry ceiling."""
        init = self._forth_definition("_EPS-RFC-INIT")
        candidate = self._forth_definition("_EPS-RFC-CANDIDATE")
        reject = self._forth_definition("_EPS-RFC-REJECT")
        private = self._forth_definition("_EPS-PRIVATE-SELECT")
        qualify = self._forth_definition("_EPS-CANDIDATE-SELECT")
        select = self._forth_definition("_EPS-SELECT-RS")
        trial = self._forth_definition("_EPS-SIGN-TRIAL")
        batch = self._forth_definition("_EPS-FOUR-TRIAL-BATCH")
        core = self._forth_definition("_EPS-PRIVATE-CORE")
        output = self._forth_definition("_EPS-OUTPUT-STATUS")
        cleanup = self._forth_definition("_EPS-CLEANUP")
        wrapper = self._forth_definition(
            "_ECDSA-P256-SHA256-SIGN-CHECKED"
        )

        for word in (private, qualify, select):
            for forbidden in ("IF", "ELSE", "THEN", "EXIT", "FCMOV"):
                self.assertIsNone(
                    re.search(
                        rf"(?<![-A-Z0-9_]){forbidden}(?![-A-Z0-9_])",
                        word,
                    )
                )
        self.assertEqual(init.count("_EPS-HMAC-INTO"), 4)
        self.assertEqual(candidate.count("_EPS-HMAC-INTO"), 1)
        self.assertEqual(reject.count("_EPS-HMAC-INTO"), 2)
        self.assertEqual(batch.count("_EPS-SIGN-TRIAL"), 4)
        self.assertEqual(batch.count("_EPS-RFC-REJECT"), 3)
        self.assertIn("_P256-SECRET-BASE-MUL-CHECKED", trial)
        for operation in ("FADD", "FMUL", "FINV"):
            self.assertIn(operation, trial)
        self.assertIn("WHILE", core)
        self.assertIn("_EPS-RFC-REJECT", core)
        self.assertNotIn("_BN256-", "\n".join((qualify, select, trial)))
        self.assertIn("_EPS-CAP @ _EPS-DER-LEN @ <", output)
        self.assertNotIn("72 <", output)
        self.assertLess(cleanup.index("_P256S-CLEANUP"),
                        cleanup.index("_EPS-FIELD-CLEAR"))
        self.assertLess(cleanup.index("_EPS-FIELD-CLEAR"),
                        cleanup.index("_EPS-WORK-WIPE"))
        self.assertIn("CATCH", wrapper)

    def test_rfc6979_private_and_nonce_range_boundaries(self):
        """Both fixed scans accept n-1 and reject zero and n exactly."""
        n = int.from_bytes(bytes([
            81, 37, 99, 252, 194, 202, 185, 243,
            132, 158, 23, 167, 173, 250, 230, 188,
            255, 255, 255, 255, 255, 255, 255, 255,
            0, 0, 0, 0, 255, 255, 255, 255,
        ]), "little")
        lines = self._store_bytes("eps-zero", bytes(32))
        lines += self._store_bytes("eps-n", n.to_bytes(32, "little"))
        lines += self._store_bytes("eps-nm1", (n - 1).to_bytes(32, "little"))
        lines += self._store_bytes("eps-n-be", n.to_bytes(32, "big"))
        lines += self._store_bytes("eps-nm1-be", (n - 1).to_bytes(32, "big"))
        lines.extend([
            "CREATE eps-h 32 ALLOT CREATE eps-out 72 ALLOT",
            "TLS-OWNER-TRY DROP",
            "eps-zero eps-h eps-out 72 _EPS-ARGS-STATUS DROP _EPS-SETUP",
            "_EPS-PRIVATE-SELECT",
            '." D0=" _EPS-KEY-VALID @ .',
            "eps-n _EPS-KEY ! _EPS-PRIVATE-SELECT",
            '." DN=" _EPS-KEY-VALID @ .',
            "eps-nm1 _EPS-KEY ! _EPS-PRIVATE-SELECT",
            '." DNM1=" _EPS-KEY-VALID @ .',
            "_EPS-V 32 0 FILL _EPS-CANDIDATE-SELECT",
            '." K0=" _EPS-CAND-VALID @ .',
            "eps-n-be _EPS-V 32 MOVE _EPS-CANDIDATE-SELECT",
            '." KN=" _EPS-CAND-VALID @ .',
            "eps-nm1-be _EPS-V 32 MOVE _EPS-CANDIDATE-SELECT",
            '." KNM1=" _EPS-CAND-VALID @ .',
            "_EPS-CLEANUP TLS-OWNER-RELEASE",
        ])
        text = self._run_kdos(lines)
        for marker in ("D0", "DN", "K0", "KN"):
            self.assertIn(f"{marker}=0 ", text)
        for marker in ("DNM1", "KNM1"):
            self.assertIn(f"{marker}=1 ", text)

    def test_rfc6979_der_trims_and_sign_pads_minimal_integers(self):
        """DER strips high zero bytes and adds only the required sign pad."""
        expected = bytes.fromhex("300702010102020080")
        lines = self._store_bytes("eps-ex", expected)
        lines.extend([
            "_EPS-WORK-WIPE 1 _EPS-R C! 128 _EPS-S C!",
            "_EPS-BUILD-DER",
            '." LEN=" _EPS-DER-LEN @ .',
            f'." DER=" _EPS-DER eps-ex {len(expected)} _XC-BYTES= .',
            "_EPS-WORK-WIPE",
        ])
        text = self._run_kdos(lines)
        self.assertIn(f"LEN={len(expected)} ", text)
        self.assertIn("DER=-1 ", text)

    def test_rfc6979_first_valid_selector_covers_all_batch_positions(self):
        """Arithmetic selection keeps the first valid result, including five."""
        lines = [
            "VARIABLE eps-want",
            ": eps-selector-case ( position -- )",
            "  eps-want ! _EPS-WORK-WIPE 0 _EPS-FOUND !",
            "  4 0 DO",
            "    _EPS-RC 32 I 1+ FILL _EPS-SC 32 I 17 + FILL",
            "    eps-want @ I 1+ = IF 1 ELSE 0 THEN _EPS-CAND-VALID !",
            "    _EPS-SELECT-RS",
            "  LOOP",
            "  eps-want @ 5 = IF",
            "    _EPS-RC 32 5 FILL _EPS-SC 32 21 FILL",
            "    1 _EPS-CAND-VALID ! _EPS-SELECT-RS",
            "  THEN",
            '  _EPS-R C@ . _EPS-S C@ . _EPS-FOUND @ . ;',
            '." P1=" 1 eps-selector-case',
            '." P2=" 2 eps-selector-case',
            '." P3=" 3 eps-selector-case',
            '." P4=" 4 eps-selector-case',
            '." P5=" 5 eps-selector-case',
            "_EPS-WORK-WIPE",
        ]
        text = self._run_kdos(lines)
        for position in range(1, 6):
            self.assertIn(
                f"P{position}={position} {position + 16} 1 ", text
            )

    def test_rfc6979_sample_signs_to_exact_canonical_der(self):
        """The RFC 6979 P-256/SHA-256 sample has exact r, s, and DER."""
        private_le = bytes.fromhex(
            "c9afa9d845ba75166b5c215767b1d693"
            "4e50c3db36e89b127b8a622b120f6721"
        )[::-1]
        digest = bytes.fromhex(
            "af2bdbe1aa9b6ec1e2ade1d694f41fc7"
            "1a831d0268e9891562113d8a62add1bf"
        )
        expected_der = bytes.fromhex(
            "3046022100efd48b2aacb6a8fd1140dd9cd45e81d6"
            "9d2c877b56aaf991c34d0ea84eaf3716022100f7cb"
            "1c942d657c41d436c7a1b6e29f65f3e900dbb9aff4"
            "064dc4ab2f843acda8"
        )
        lines = self._store_bytes("eps-d", private_le)
        lines += self._store_bytes("eps-h", digest)
        lines += self._store_bytes("eps-der", expected_der)
        lines.extend([
            "CREATE eps-out 72 ALLOT eps-out 72 165 FILL",
            "CREATE eps-zero 32 ALLOT CREATE eps-acc 32 ALLOT",
            "CREATE eps-lo 32 ALLOT CREATE eps-hi 32 ALLOT",
            "eps-zero 32 0 FILL",
            "VARIABLE eps-len VARIABLE eps-ior VARIABLE eps-t0 VARIABLE eps-cyc",
            "PERF-CYCLES eps-t0 !",
            "eps-d eps-h eps-out 72 _ECDSA-P256-SHA256-SIGN-CHECKED",
            "eps-ior ! eps-len !",
            '." SIGN-DEPTH=" DEPTH .',
            "PERF-CYCLES eps-t0 @ - eps-cyc !",
            '." IOR=" eps-ior @ .',
            '." LEN=" eps-len @ .',
            '." CYC=" eps-cyc @ .',
            f'." DER=" eps-out eps-der {len(expected_der)} _XC-BYTES= .',
            ': eps-work-zero? ( -- flag ) TRUE /ECDSA-P256-SIGN-WORK 0 DO',
            '  _ECDSA-P256-SIGN-WORK I + C@ 0= AND LOOP ;',
            ': eps-p256-zero? ( -- flag ) TRUE /P256-SECRET-WORK 0 DO',
            '  _P256-SECRET-WORK I + C@ 0= AND LOOP ;',
            ': eps-hmac-zero? ( -- flag ) TRUE',
            '  64 0 DO HMAC256-IPAD I + C@ 0= AND LOOP',
            '  64 0 DO HMAC256-OPAD I + C@ 0= AND LOOP',
            '  32 0 DO HMAC256-INNER I + C@ 0= AND LOOP',
            '  32 0 DO HMAC256-KEY I + C@ 0= AND LOOP ;',
            '." WORK=" eps-work-zero? .',
            '." P256=" eps-p256-zero? .',
            '." HMAC=" eps-hmac-zero? .',
            '." OWNER=" TLS-OWNER-DEPTH @ .',
            "eps-acc 32 90 FILL eps-acc GF-R@",
            "eps-lo 32 90 FILL eps-hi 32 90 FILL",
            "eps-zero eps-zero eps-lo eps-hi FMUL-ADD-RAW",
            "eps-zero eps-acc _EC-T1 FCEQ",
            '." ACC=" _EC-T1 C@ .',
            "eps-zero eps-lo _EC-T1 FCEQ",
            '." PREVLO=" _EC-T1 C@ .',
            "eps-zero eps-hi _EC-T1 FCEQ",
            '." PREVHI=" _EC-T1 C@ .',
        ])
        text = self._run_kdos(lines)
        self.assertIn("IOR=0 ", text)
        self.assertIn(f"LEN={len(expected_der)} ", text)
        self.assertIn("SIGN-DEPTH=0 ", text)
        self.assertIn("DER=-1 ", text)
        self.assertIn("WORK=-1 ", text)
        self.assertIn("P256=-1 ", text)
        self.assertIn("HMAC=-1 ", text)
        self.assertIn("OWNER=0 ", text)
        for marker in ("ACC", "PREVLO", "PREVHI"):
            self.assertIn(f"{marker}=1 ", text)

    def test_rfc6979_test_vector_uses_its_exact_71_byte_capacity(self):
        """Canonical DER accepts the actual bound; it does not require 72."""
        private_le = bytes.fromhex(
            "c9afa9d845ba75166b5c215767b1d693"
            "4e50c3db36e89b127b8a622b120f6721"
        )[::-1]
        digest = bytes.fromhex(
            "9f86d081884c7d659a2feaa0c55ad015"
            "a3bf4f1b2b0b822cd15d6c15b0f00a08"
        )
        expected_der = bytes.fromhex(
            "3045022100f1abb023518351cd71d881567b1ea663"
            "ed3efcf6c5132b354f28d3b0b7d383670220019f"
            "4113742a2b14bd25926b49c649155f267e60d3814"
            "b4c0cc84250e46f0083"
        )
        lines = self._store_bytes("eps-d", private_le)
        lines += self._store_bytes("eps-h", digest)
        lines += self._store_bytes("eps-der", expected_der)
        lines.extend([
            f"CREATE eps-out {len(expected_der)} ALLOT",
            f"eps-out {len(expected_der)} 165 FILL",
            f"eps-d eps-h eps-out {len(expected_der)} "
            "_ECDSA-P256-SHA256-SIGN-CHECKED",
            '." IOR=" . ." LEN=" .',
            f'." DER=" eps-out eps-der {len(expected_der)} _XC-BYTES= .',
        ])
        text = self._run_kdos(lines)
        self.assertIn("IOR=0 LEN=71 ", text)
        self.assertIn("DER=-1 ", text)

    def test_rfc6979_private_core_has_equal_architectural_cycles(self):
        """Two ordinary first-batch signatures execute equal private cycles."""
        private_le = bytes.fromhex(
            "c9afa9d845ba75166b5c215767b1d693"
            "4e50c3db36e89b127b8a622b120f6721"
        )[::-1]
        sample_digest = bytes.fromhex(
            "af2bdbe1aa9b6ec1e2ade1d694f41fc7"
            "1a831d0268e9891562113d8a62add1bf"
        )
        test_digest = bytes.fromhex(
            "9f86d081884c7d659a2feaa0c55ad015"
            "a3bf4f1b2b0b822cd15d6c15b0f00a08"
        )
        def measure(digest: bytes) -> int:
            lines = self._store_bytes("eps-d", private_le)
            lines += self._store_bytes("eps-h", digest)
            lines.extend([
                "CREATE eps-out 72 ALLOT",
                "VARIABLE eps-t0 VARIABLE eps-cyc",
                "TLS-OWNER-TRY DROP",
                "eps-d eps-h eps-out 72 _EPS-ARGS-STATUS DROP _EPS-SETUP",
                "PERF-CYCLES eps-t0 ! _EPS-PRIVATE-CORE",
                "PERF-CYCLES eps-t0 @ - eps-cyc !",
                "_EPS-CLEANUP TLS-OWNER-RELEASE",
                '." CYC=" eps-cyc @ .',
                '." OWNER=" TLS-OWNER-DEPTH @ .',
            ])
            text = self._run_kdos(lines)
            self.assertIn("OWNER=0 ", text)
            return int(text.split("CYC=")[-1].split()[0])

        # Each measurement begins from the same pristine networking snapshot;
        # comparing back-to-back calls would include emulator timer/JIT phase.
        self.assertEqual(measure(sample_digest), measure(test_digest))

    def test_rfc6979_failures_are_atomic_and_capacity_is_exact(self):
        """Invalid key, short capacity, and aliases never publish bytes."""
        private_le = bytes.fromhex(
            "c9afa9d845ba75166b5c215767b1d693"
            "4e50c3db36e89b127b8a622b120f6721"
        )[::-1]
        digest = bytes.fromhex(
            "af2bdbe1aa9b6ec1e2ade1d694f41fc7"
            "1a831d0268e9891562113d8a62add1bf"
        )
        alias_zone = private_le + bytes([0xA6]) * 40
        lines = self._store_bytes("eps-d", private_le)
        lines += self._store_bytes("eps-alias", alias_zone)
        lines += self._store_bytes("eps-alias-copy", alias_zone)
        lines += self._store_bytes("eps-h", digest)
        lines.extend([
            "CREATE eps-zero 32 ALLOT CREATE eps-out 72 ALLOT",
            ': eps-reset eps-out 72 165 FILL ;',
            ': eps-atomic? TRUE 72 0 DO eps-out I + C@ 165 = AND LOOP ;',
            "eps-reset eps-zero eps-h eps-out 72",
            "_ECDSA-P256-SHA256-SIGN-CHECKED",
            '." KEY-IOR=" . ." KEY-LEN=" .',
            '." KEY-ATOMIC=" eps-atomic? .',
            "eps-reset eps-d eps-h eps-out 71",
            "_ECDSA-P256-SHA256-SIGN-CHECKED",
            '." CAP-IOR=" . ." CAP-LEN=" .',
            '." CAP-ATOMIC=" eps-atomic? .',
            "eps-alias eps-h eps-alias 72 _ECDSA-P256-SHA256-SIGN-CHECKED",
            '." ALIAS-IOR=" . ." ALIAS-LEN=" .',
            '." ALIAS-PRESERVED=" eps-alias eps-alias-copy 72 _XC-BYTES= .',
            "eps-d eps-h _P256-N-INV 72 _ECDSA-P256-SHA256-SIGN-CHECKED",
            '." NINV-IOR=" . ." NINV-LEN=" .',
            ': eps-ninv-zero? TRUE 32 0 DO',
            '  _P256-N-INV I + C@ 0= AND LOOP ;',
            '." NINV-ZERO=" eps-ninv-zero? .',
            "eps-reset TLS-OWNER-TRY DROP",
            "eps-d eps-h eps-out 72 _ECDSA-P256-SHA256-SIGN-CHECKED",
            '." NEST-IOR=" . ." NEST-LEN=" .',
            '." NEST-DEPTH=" TLS-OWNER-DEPTH @ .',
            "TLS-OWNER-RELEASE",
            '." OWNER=" TLS-OWNER-DEPTH @ .',
        ])
        text = self._run_kdos(lines)
        self.assertIn("KEY-IOR=-4313 KEY-LEN=0 ", text)
        self.assertIn("KEY-ATOMIC=-1 ", text)
        self.assertIn("CAP-IOR=-4314 CAP-LEN=0 ", text)
        self.assertIn("CAP-ATOMIC=-1 ", text)
        self.assertIn("ALIAS-IOR=-4312 ALIAS-LEN=0 ", text)
        self.assertIn("ALIAS-PRESERVED=-1 ", text)
        self.assertIn("NINV-IOR=-4312 NINV-LEN=0 ", text)
        self.assertIn("NINV-ZERO=-1 ", text)
        self.assertIn("NEST-IOR=0 NEST-LEN=72 ", text)
        self.assertIn("NEST-DEPTH=1 ", text)
        self.assertIn("OWNER=0 ", text)

    def test_rfc6979_busy_and_early_alias_paths_do_not_mutate_owner(self):
        """Busy is non-mutating; internal input aliases fail after cleanup."""
        private_le = bytes.fromhex(
            "c9afa9d845ba75166b5c215767b1d693"
            "4e50c3db36e89b127b8a622b120f6721"
        )[::-1]
        digest = bytes.fromhex(
            "af2bdbe1aa9b6ec1e2ade1d694f41fc7"
            "1a831d0268e9891562113d8a62add1bf"
        )
        lines = self._store_bytes("eps-d", private_le)
        lines += self._store_bytes("eps-h", digest)
        lines.extend([
            "CREATE eps-out 72 ALLOT eps-out 72 165 FILL",
            ': eps-poison? TRUE /ECDSA-P256-SIGN-WORK 0 DO',
            '  _ECDSA-P256-SIGN-WORK I + C@ 90 = AND LOOP ;',
            ': eps-p256-poison? TRUE /P256-SECRET-WORK 0 DO',
            '  _P256-SECRET-WORK I + C@ 91 = AND LOOP ;',
            ': eps-zero? TRUE /ECDSA-P256-SIGN-WORK 0 DO',
            '  _ECDSA-P256-SIGN-WORK I + C@ 0= AND LOOP ;',
            ': eps-p256-zero? TRUE /P256-SECRET-WORK 0 DO',
            '  _P256-SECRET-WORK I + C@ 0= AND LOOP ;',
            ': eps-out-atomic? TRUE 72 0 DO',
            '  eps-out I + C@ 165 = AND LOOP ;',
            "TLS-OWNER-TRY DROP TASK-ID 1+ TLS-OWNER-TASK !",
            "_ECDSA-P256-SIGN-WORK /ECDSA-P256-SIGN-WORK 90 FILL",
            "_P256-SECRET-WORK /P256-SECRET-WORK 91 FILL",
            "eps-d eps-h eps-out 72 _ECDSA-P256-SHA256-SIGN-CHECKED",
            '." BUSY-IOR=" . ." BUSY-LEN=" .',
            '." POISON=" eps-poison? .',
            '." P256-POISON=" eps-p256-poison? .',
            '." BUSY-ATOMIC=" eps-out-atomic? .',
            '." HELD=" TLS-OWNER-DEPTH @ .',
            "TASK-ID TLS-OWNER-TASK ! TLS-OWNER-RELEASE",
            "eps-out 72 165 FILL",
            "_ECDSA-P256-SIGN-WORK eps-h eps-out 72",
            "_ECDSA-P256-SHA256-SIGN-CHECKED",
            '." ALIAS-IOR=" . ." ALIAS-LEN=" .',
            '." ZERO=" eps-zero? .',
            '." P256-ZERO=" eps-p256-zero? .',
            '." ALIAS-ATOMIC=" eps-out-atomic? .',
            '." OWNER=" TLS-OWNER-DEPTH @ .',
        ])
        text = self._run_kdos(lines)
        self.assertIn("BUSY-IOR=-4206 BUSY-LEN=0 ", text)
        self.assertIn("POISON=-1 ", text)
        self.assertIn("P256-POISON=-1 ", text)
        self.assertIn("BUSY-ATOMIC=-1 ", text)
        self.assertIn("HELD=1 ", text)
        self.assertIn("ALIAS-IOR=-4312 ALIAS-LEN=0 ", text)
        self.assertIn("ZERO=-1 ", text)
        self.assertIn("P256-ZERO=-1 ", text)
        self.assertIn("ALIAS-ATOMIC=-1 ", text)
        self.assertIn("OWNER=0 ", text)

    def test_rfc6979_unwind_rethrows_after_complete_cleanup(self):
        """Unexpected Forth exceptions scrub both private lanes and unlock."""
        lines = [
            ': eps-work-zero? TRUE /ECDSA-P256-SIGN-WORK 0 DO',
            '  _ECDSA-P256-SIGN-WORK I + C@ 0= AND LOOP ;',
            ': eps-p256-zero? TRUE /P256-SECRET-WORK 0 DO',
            '  _P256-SECRET-WORK I + C@ 0= AND LOOP ;',
            ': eps-unwind-probe -4378 _EPS-UNWIND ;',
            "TLS-OWNER-TRY DROP",
            "_ECDSA-P256-SIGN-WORK /ECDSA-P256-SIGN-WORK 90 FILL",
            "_P256-SECRET-WORK /P256-SECRET-WORK 90 FILL",
            "' eps-unwind-probe CATCH",
            '." THROW=" .',
            '." WORK=" eps-work-zero? .',
            '." P256=" eps-p256-zero? .',
            '." OWNER=" TLS-OWNER-DEPTH @ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("THROW=-4378 ", text)
        self.assertIn("WORK=-1 ", text)
        self.assertIn("P256=-1 ", text)
        self.assertIn("OWNER=0 ", text)

    def test_rfc6979_cancellation_waits_for_a_complete_batch(self):
        """An armed generation cancels atomically after four real trials."""
        private_le = bytes.fromhex(
            "c9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721"
        )[::-1]
        digest = bytes.fromhex(
            "af2bdbe1aa9b6ec1e2ade1d694f41fc7"
            "1a831d0268e9891562113d8a62add1bf"
        )
        core = self._forth_definition("_EPS-PRIVATE-CORE")
        self.assertLess(core.index("_EPS-FOUR-TRIAL-BATCH"),
                        core.index("_EPS-CANCELLED?"))
        self.assertEqual(core.count("_EPS-CANCELLED?"), 1)

        lines = self._store_bytes("eps-cancel-d", private_le)
        lines += self._store_bytes("eps-cancel-h", digest)
        lines.extend([
            "CREATE eps-cancel-out 72 ALLOT eps-cancel-out 72 165 FILL",
            "VARIABLE eps-cancel-request 1 eps-cancel-request !",
            ": eps-cancel-out-atomic? TRUE 72 0 DO",
            "  eps-cancel-out I + C@ 165 = AND LOOP ;",
            ": eps-cancel-work-zero? TRUE /ECDSA-P256-SIGN-WORK 0 DO",
            "  _ECDSA-P256-SIGN-WORK I + C@ 0= AND LOOP ;",
            ": eps-cancel-p256-zero? TRUE /P256-SECRET-WORK 0 DO",
            "  _P256-SECRET-WORK I + C@ 0= AND LOOP ;",
            "eps-cancel-request _EPS-CANCEL-A !",
            "1 _EPS-CANCEL-GENERATION !",
            "eps-cancel-d eps-cancel-h eps-cancel-out 72",
            "_ECDSA-P256-SHA256-SIGN-CHECKED",
            '." IOR=" . ." LEN=" .',
            '." ATOMIC=" eps-cancel-out-atomic? .',
            '." WORK=" eps-cancel-work-zero? .',
            '." P256=" eps-cancel-p256-zero? .',
            '." ARMED=" _EPS-CANCEL-A @ .',
            '." GENERATION=" _EPS-CANCEL-GENERATION @ .',
            '." OWNER=" TLS-OWNER-DEPTH @ .',
        ])
        text = self._run_kdos(lines)
        self.assertIn("IOR=-4317 LEN=0 ", text)
        self.assertIn("ATOMIC=-1 ", text)
        self.assertIn("WORK=-1 ", text)
        self.assertIn("P256=-1 ", text)
        self.assertIn("ARMED=0 ", text)
        self.assertIn("GENERATION=0 ", text)
        self.assertIn("OWNER=0 ", text)

    def test_ecdsa_decode_sig_valid(self):
        """ECDSA-DECODE-SIG decodes a DER-encoded signature into r, s."""
        # Simple test: r=1, s=2
        sig_der = bytes([
            0x30, 0x06,                   # SEQUENCE, length 6
            0x02, 0x01, 0x01,             # INTEGER r=1
            0x02, 0x01, 0x02,             # INTEGER s=2
        ])
        lines = self._store_bytes("tv-sig", sig_der)
        lines.extend([
            f"tv-sig {len(sig_der)} ECDSA-DECODE-SIG",
            '." RET=" .',
            '." R0=" _ECDSA-R C@ .',
            '." S0=" _ECDSA-S C@ .',
        ])
        text = self._run_kdos(lines)
        self.assertIn("RET=0 ", text)     # success
        self.assertIn("R0=1 ", text)      # r=1 in last byte (right-aligned)
        self.assertIn("S0=2 ", text)      # s=2

    def test_ecdsa_decode_sig_rejects_bad(self):
        """ECDSA-DECODE-SIG rejects non-SEQUENCE."""
        lines = self._store_bytes("tv-bad", bytes([0x02, 0x01, 0x00]))
        lines.extend([
            "tv-bad 3 ECDSA-DECODE-SIG",
            '." RET=" .',
        ])
        text = self._run_kdos(lines)
        self.assertTrue("RET=-1 " in text or "RET=18446744073709551615 " in text)

    def test_ec_double_identity(self):
        """EC-DOUBLE of a known point produces a valid non-zero result."""
        # Use P-256 generator G, double it, check Z != 0
        lines = [
            "PRIME-P256",
            "P256-GX P256-GY _EC-ONE",           # Jacobian (Gx, Gy, 1)
            "_EC-RX _EC-RY _EC-RZ EC-DOUBLE",
            '." RZ0=" _EC-RZ C@ .',               # Z should be non-zero
        ]
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        rz0 = int(text.split("RZ0=")[-1].split()[0])
        self.assertNotEqual(rz0, 0, "Z coordinate should be non-zero after doubling G")

    def test_ec_add_inf_plus_p(self):
        """EC-ADD with P1=infinity returns P2."""
        lines = [
            "PRIME-P256",
            # P1 = infinity (0, 1, 0)
            "_EC-ZERO _EC-ONE _EC-ZERO",
            # P2 = generator G (Gx, Gy, 1)
            "P256-GX P256-GY _EC-ONE",
            "_EC-RX _EC-RY _EC-RZ EC-ADD",
            # Result should equal G
            "P256-GX _EC-RX _EC-T1 FCEQ",
            '." XEQGX=" _EC-T1 C@ .',
            "P256-GY _EC-RY _EC-T1 FCEQ",
            '." YEQGY=" _EC-T1 C@ .',
        ]
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        # FCEQ stores nonzero if equal
        xeq = int(text.split("XEQGX=")[-1].split()[0])
        yeq = int(text.split("YEQGY=")[-1].split()[0])
        self.assertNotEqual(xeq, 0, "X should equal Gx")
        self.assertNotEqual(yeq, 0, "Y should equal Gy")

    def test_ecdsa_verifies_certificate_signature(self):
        """ECDSA-P256-VERIFY accepts a real SHA-256 certificate signature."""
        pubkey = TestKDOSX509._PUB_KEY
        digest = bytes.fromhex(
            "b213ca91f5dcd3d45fbe66562ab30eaf"
            "2dc9e89227656de3bd29b389f4e88530"
        )
        # The fixture certificate's outer signatureValue, without the BIT
        # STRING unused-bits byte.
        signature = bytes([
            0x30, 0x45, 0x02, 0x20, 0x77, 0x23, 0xD8, 0x4E,
            0x33, 0x1D, 0x96, 0xBE, 0xD7, 0x30, 0xB2, 0x1C,
            0xEE, 0xF0, 0xC1, 0x43, 0x41, 0x36, 0xE0, 0x53,
            0x5F, 0xC9, 0xAA, 0x6A, 0xE5, 0x56, 0x62, 0x8E,
            0xF7, 0xE3, 0xFE, 0x6C, 0x02, 0x21, 0x00, 0x9F,
            0x51, 0xDD, 0x3D, 0xA5, 0x3C, 0x8F, 0x81, 0x8C,
            0xAF, 0x46, 0x67, 0xF8, 0x4C, 0xF5, 0xEF, 0x20,
            0x7B, 0xE5, 0xC2, 0x57, 0x92, 0xE9, 0xFA, 0x31,
            0x00, 0x54, 0x10, 0xAD, 0x69, 0x26, 0x86,
        ])
        lines = self._store_bytes("tv-hash", digest)
        lines += self._store_bytes("tv-pub", pubkey)
        lines += self._store_bytes("tv-sig", signature)
        lines.extend([
            f"tv-hash tv-pub tv-sig {len(signature)} ECDSA-P256-VERIFY",
            '." VALID=" .',
            "tv-sig 8 + DUP C@ 1 XOR SWAP C!",
            f"tv-hash tv-pub tv-sig {len(signature)} ECDSA-P256-VERIFY",
            '." CORRUPT=" .',
        ])
        text = self._run_kdos(lines, max_steps=5_000_000_000)
        self.assertIn("VALID=0 ", text)
        self.assertTrue(
            "CORRUPT=-1 " in text or
            "CORRUPT=18446744073709551615 " in text
        )


class TestKDOSRSA(_KDOSNetworkTestBase):
    """Multi-precision Montgomery arithmetic, RSA of every width, DER and
    padding, and TLS dispatch."""

    FIXTURE_DIR = os.path.join(PROJECT_ROOT, "tests", "fixtures", "tls")
    NOW = 1_767_225_600

    @classmethod
    def _fixture(cls, name: str) -> bytes:
        with open(os.path.join(cls.FIXTURE_DIR, f"{name}.b64"), "rb") as stream:
            return base64.b64decode(stream.read())

    @staticmethod
    def _forth_bytes(name: str, data: bytes) -> list[str]:
        lines = [f"CREATE {name}"]
        for offset in range(0, len(data), 16):
            lines.append(" ".join(f"{byte} C," for byte in data[offset:offset + 16]))
        return lines

    @classmethod
    def _key_lines(cls, name: str, modulus: bytes,
                   exponent: bytes = b"\x01\x00\x01") -> list[str]:
        """An RSA-KEY record called name over the given modulus."""
        lines = cls._forth_bytes(f"{name}-n", modulus)
        lines += cls._forth_bytes(f"{name}-e", exponent)
        lines += [
            f"CREATE {name} /RSA-KEY ALLOT",
            f"{name}-n {name} RSAK.N + ! {len(modulus)} {name} RSAK.N-U + !",
            f"{name}-e {name} RSAK.E + ! {len(exponent)} {name} RSAK.E-U + !",
        ]
        return lines

    @staticmethod
    def _rsa_modulus(cert: bytes) -> bytes:
        marker = bytes.fromhex("0282010100")
        start = cert.index(marker) + len(marker)
        return cert[start:start + 256]

    @staticmethod
    def _der(tag: int, value: bytes) -> bytes:
        length = len(value)
        if length < 128:
            encoded = bytes([length])
        else:
            raw = length.to_bytes((length.bit_length() + 7) // 8, "big")
            encoded = bytes([0x80 | len(raw)]) + raw
        return bytes([tag]) + encoded + value

    @classmethod
    def _rsa_spki(cls, modulus: bytes, exponent=b"\x01\x00\x01") -> bytes:
        algorithm = cls._der(
            0x30,
            bytes.fromhex("06092a864886f70d0101010500"),
        )
        public = cls._der(
            0x30,
            cls._der(0x02, b"\x00" + modulus) + cls._der(0x02, exponent),
        )
        return cls._der(0x30, algorithm + cls._der(0x03, b"\x00" + public))

    @staticmethod
    def _bundle(root: bytes, scope=b"rsa.test.example") -> bytes:
        return mpta_bundle(mpta_record(root, scope))

    @staticmethod
    def _mgf1_sha256(seed: bytes, length: int) -> bytes:
        result = bytearray()
        for counter in range((length + 31) // 32):
            result += hashlib.sha256(seed + counter.to_bytes(4, "big")).digest()
        return bytes(result[:length])

    @classmethod
    def _pss_em(cls, message_hash: bytes, salt: bytes, *,
                pad_byte=0, delimiter=1, trailer=0xBC,
                corrupt_salt=False) -> bytes:
        db = bytearray([pad_byte] + [0] * 189 + [delimiter])
        db += salt
        h = hashlib.sha256(b"\x00" * 8 + message_hash + salt).digest()
        if corrupt_salt:
            db[-1] ^= 1
        mask = cls._mgf1_sha256(h, 223)
        masked = bytearray(left ^ right for left, right in zip(db, mask))
        masked[0] &= 0x7F
        return bytes(masked) + h + bytes([trailer])

    def test_montgomery_differential_and_high_limb_subtraction(self):
        """MPM-MUL agrees with Python at 1 to 16 digits, including the
        final subtraction for operands just below the modulus."""
        rng = random.Random(0x4D503634)
        moduli = [
            (1, (1 << 256) - 189),
            (2, (1 << 384) - (1 << 128) - (1 << 96) + (1 << 32) - 1),
            (8, (1 << 2048) - 159),
            (12, rng.getrandbits(3072) | 1 | (1 << 3071)),
            (16, rng.getrandbits(4096) | 1 | (1 << 4095)),
        ]
        lines = []
        tokens = []
        for digits, n in moduli:
            width = 32 * digits
            r_inv = pow(1 << (256 * digits), -1, n)
            r2 = pow(1 << (256 * digits), 2, n)
            lines += self._forth_bytes(f"tv-n{digits}", n.to_bytes(width, "little"))
            lines += self._forth_bytes(f"tv-r2{digits}", r2.to_bytes(width, "little"))
            lines += [
                f"CREATE tv-ctx{digits} {digits} MPM-CONTEXT-SIZE ALLOT",
                f"CREATE tv-o{digits} {width} ALLOT",
                f'tv-n{digits} {digits} tv-ctx{digits} MPM-INIT ." I{digits}=" .',
                f'." R{digits}=" tv-ctx{digits} MPM.R2 + @ tv-r2{digits} '
                f"{width} _XC-BYTES= .",
            ]
            tokens += [f"I{digits}=0 ", f"R{digits}=-1 "]
            cases = [(n - 1, n - 1)]
            cases += [(rng.randrange(n), rng.randrange(n)) for _ in range(2)]
            for index, (left, right) in enumerate(cases):
                tag = f"{digits}-{index}"
                expected = left * right * r_inv % n
                lines += self._forth_bytes(f"tv-a{tag}", left.to_bytes(width, "little"))
                lines += self._forth_bytes(f"tv-b{tag}", right.to_bytes(width, "little"))
                lines += self._forth_bytes(f"tv-e{tag}", expected.to_bytes(width, "little"))
                lines.extend([
                    f"tv-a{tag} tv-b{tag} tv-o{digits} tv-ctx{digits} MPM-MUL",
                    f'." M{tag}=" tv-o{digits} tv-e{tag} {width} _XC-BYTES= .',
                ])
                tokens.append(f"M{tag}=-1 ")
        text = self._run_kdos(lines, max_steps=1_500_000_000)
        for token in tokens:
            self.assertIn(token, text)

    def test_montgomery_add_sub_exp_and_rejections(self):
        """Modular add, subtract and exponentiation, and the moduli
        MPM-INIT refuses."""
        n = (1 << 384) - (1 << 128) - (1 << 96) + (1 << 32) - 1
        a, b = n - 5, 7
        exponent = bytes.fromhex("0100ff03")
        base = 0x1234567890ABCDEF << 200
        r = 1 << 512
        lines = self._forth_bytes("tv-n", n.to_bytes(64, "little"))
        lines += self._forth_bytes("tv-a", a.to_bytes(64, "little"))
        lines += self._forth_bytes("tv-b", b.to_bytes(64, "little"))
        lines += self._forth_bytes("tv-sum", ((a + b) % n).to_bytes(64, "little"))
        lines += self._forth_bytes("tv-dif", ((b - a) % n).to_bytes(64, "little"))
        lines += self._forth_bytes("tv-x", (base * r % n).to_bytes(64, "little"))
        lines += self._forth_bytes(
            "tv-pow",
            (pow(base, int.from_bytes(exponent, "big"), n) * r % n).to_bytes(64, "little"),
        )
        lines += self._forth_bytes("tv-one", (r % n).to_bytes(64, "little"))
        lines += self._forth_bytes("tv-exp", exponent)
        lines += self._forth_bytes("tv-even", (n - 1).to_bytes(64, "little"))
        # An odd low digit under a zero top digit.
        lines += self._forth_bytes(
            "tv-top0", (n % (1 << 256)).to_bytes(32, "little") + bytes(32)
        )
        lines.extend([
            "CREATE tv-ctx 2 MPM-CONTEXT-SIZE ALLOT CREATE tv-o 64 ALLOT",
            'tv-n 2 tv-ctx MPM-INIT ." I=" .',
            "tv-a tv-b tv-o tv-ctx MPM-ADD",
            '." ADD=" tv-o tv-sum 64 _XC-BYTES= .',
            "tv-b tv-a tv-o tv-ctx MPM-SUB",
            '." SUB=" tv-o tv-dif 64 _XC-BYTES= .',
            "tv-x tv-exp 4 tv-o tv-ctx MPM-EXP",
            '." EXP=" tv-o tv-pow 64 _XC-BYTES= .',
            "tv-x tv-exp 0 tv-o tv-ctx MPM-EXP",
            '." EXP0=" tv-o tv-one 64 _XC-BYTES= .',
            "CREATE tv-ctx2 2 MPM-CONTEXT-SIZE ALLOT",
            'tv-even 2 tv-ctx2 MPM-INIT ." EVEN=" .',
            'tv-top0 2 tv-ctx2 MPM-INIT ." TOP0=" .',
            'tv-n 0 tv-ctx2 MPM-INIT ." ZERO=" .',
        ])
        text = self._run_kdos(lines, max_steps=900_000_000)
        for token in ("I=0 ", "ADD=-1 ", "SUB=-1 ", "EXP=-1 ", "EXP0=-1 ",
                      "EVEN=-1 ", "TOP0=-1 ", "ZERO=-1 "):
            self.assertIn(token, text)

    def test_field_raw_multiply_accumulate_carries_into_the_high_half(self):
        """FMUL-ADD-RAW's 512-bit sum carries out of bit 255 on every backend."""
        ones = ((1 << 256) - 1).to_bytes(32, "little")
        lines = self._forth_bytes("tv-ones", ones)
        lines.extend([
            "CREATE tv-lo 32 ALLOT CREATE tv-hi 32 ALLOT",
            "tv-ones _MPM-ONE tv-lo tv-hi FMUL-RAW",
            "tv-ones _MPM-ONE tv-lo tv-hi FMUL-ADD-RAW",
            '." LO0=" tv-lo C@ . ." LO1=" tv-lo 1+ C@ .',
            '." HI0=" tv-hi C@ . ." HI1=" tv-hi 1+ C@ .',
        ])
        text = self._run_kdos(lines)
        for token in ("LO0=254 ", "LO1=255 ", "HI0=1 ", "HI1=0 "):
            self.assertIn(token, text)

    def test_public_differential_and_busy_gate(self):
        """RSA-PUBLIC matches Python at three widths; a busy module refuses."""
        rng = random.Random(0x52534131)
        lines = []
        tokens = []
        for bits in (2048, 3072, 4096):
            n = rng.getrandbits(bits) | 1 | (1 << (bits - 1))
            signature = rng.randrange(n)
            expected = pow(signature, 65537, n).to_bytes(bits // 8, "big")
            lines += self._key_lines(f"tv-k{bits}", n.to_bytes(bits // 8, "big"))
            lines += self._forth_bytes(f"tv-s{bits}", signature.to_bytes(bits // 8, "big"))
            lines += self._forth_bytes(f"tv-x{bits}", expected)
            lines.extend([
                f"CREATE tv-o{bits} {bits // 8} ALLOT",
                f'tv-s{bits} {bits // 8} tv-k{bits} tv-o{bits} RSA-PUBLIC ." P{bits}=" .',
                f'." M{bits}=" tv-o{bits} tv-x{bits} {bits // 8} _XC-BYTES= .',
            ])
            tokens += [f"P{bits}=0 ", f"M{bits}=-1 "]
        lines.extend([
            "-1 _RSA-BUSY !",
            'tv-s2048 256 tv-k2048 tv-o2048 RSA-PUBLIC ." BUSY=" .',
            "0 _RSA-BUSY !",
            'RSA-E-BUSY ." BUSYCODE=" .',
        ])
        text = self._run_kdos(lines, max_steps=900_000_000)
        for token in tokens + ["BUSY=-2 ", "BUSYCODE=-2 "]:
            self.assertIn(token, text)

    def test_public_rejects_malformed_keys(self):
        """An even modulus, a zero top byte, a missing or oversized exponent,
        and a short signature are refused."""
        n = ((1 << 2048) - 159).to_bytes(256, "big")
        even = n[:-1] + bytes([n[-1] & 0xFE])
        lines = self._key_lines("tv-k", n)
        lines += self._key_lines("tv-even", even)
        lines += self._key_lines("tv-lead", b"\x00" + n[1:])
        lines += self._forth_bytes("tv-s", (17).to_bytes(256, "big"))
        lines.extend([
            "CREATE tv-o 256 ALLOT",
            'tv-s 256 tv-even tv-o RSA-PUBLIC ." EVEN=" .',
            'tv-s 256 tv-lead tv-o RSA-PUBLIC ." LEAD=" .',
            "0 tv-k RSAK.E-U + !",
            'tv-s 256 tv-k tv-o RSA-PUBLIC ." NOEXP=" .',
            "257 tv-k RSAK.E-U + !",
            'tv-s 256 tv-k tv-o RSA-PUBLIC ." LONGEXP=" .',
            "3 tv-k RSAK.E-U + !",
            'tv-s 255 tv-k tv-o RSA-PUBLIC ." SHORT=" .',
            'tv-s 256 tv-k tv-o RSA-PUBLIC ." OK=" .',
        ])
        text = self._run_kdos(lines, max_steps=900_000_000)
        for token in ("EVEN=-1 ", "LEAD=-1 ", "NOEXP=-1 ", "LONGEXP=-1 ",
                      "SHORT=-1 ", "OK=0 "):
            self.assertIn(token, text)

    def test_public_rejects_signature_equal_to_modulus(self):
        n = ((1 << 2048) - 159).to_bytes(256, "big")
        lines = self._key_lines("tv-k", n)
        lines.extend([
            "CREATE tv-o 256 ALLOT",
            'tv-k-n 256 tv-k tv-o RSA-PUBLIC ." FLAG=" .',
        ])
        text = self._run_kdos(lines)
        self.assertIn("FLAG=-1 ", text)

    def test_rsa_spki_and_signature_algorithm_are_strict(self):
        """Invalid RSA keys are malformed (-1); valid keys outside the
        profile, 2048 to 8192 bits in whole bytes with an exponent below
        2^33, are unsupported (-2)."""
        modulus = ((1 << 2048) - 159).to_bytes(256, "big")
        valid = self._rsa_spki(modulus)
        bad_null = valid.replace(b"\x05\x00", b"\x04\x00", 1)
        odd_width = valid.replace(bytes.fromhex("0282010100"),
                                  bytes.fromhex("0282010101"), 1)
        lead_zero = self._rsa_spki(modulus, b"\x00\x01\x00\x01")
        even = self._rsa_spki(modulus[:-1] + bytes([modulus[-1] & 0xFE]))
        cases = {
            "v": valid,
            "n": bad_null,
            "w": odd_width,
            "z": lead_zero,
            "o": even,
            "a": self._rsa_spki(modulus, b"\x03"),
            "b": self._rsa_spki(modulus, b"\x01"),
            "c": self._rsa_spki(modulus, b"\x01\x00\x00\x01"),
            "d": self._rsa_spki(modulus, (2**33 - 1).to_bytes(5, "big")),
            "f": self._rsa_spki(modulus, (2**33 + 1).to_bytes(5, "big")),
            "g": self._rsa_spki(modulus, b"\x01\x00\x03"),
            "h": self._rsa_spki(b"\x80" + b"\x00" * 510 + b"\x01"),
            "i": self._rsa_spki(b"\x80" + b"\x00" * 1022 + b"\x01"),
            "j": self._rsa_spki(b"\x80" + b"\x00" * 1023 + b"\x01"),
            "k": self._rsa_spki(b"\x80" + b"\x00" * 253 + b"\x01"),
        }
        lines = []
        for name, data in cases.items():
            lines += self._forth_bytes(f"tv-{name}", data)
            lines.append(f"CREATE tv-d{name} /X509-CERT ALLOT")
            lines.append(
                f'." {name.upper()}=" tv-{name} tv-{name} {len(data)} + '
                f"tv-d{name} X509-PARSE-SPKI-DESC . DROP"
            )
        lines += [
            '." VKEY=" tv-dv XC.PUB-ALGO + @ . tv-dv XC.PUB-U + @ .'
            " tv-dv XC.PUB-E-U + @ .",
            '." HKEY=" tv-dh XC.PUB-U + @ .',
        ]
        alg = bytes.fromhex("300d06092a864886f70d01010b0500")
        alg_no_null = bytes.fromhex("300b06092a864886f70d01010b")
        sha1 = bytes.fromhex("300d06092a864886f70d0101050500")
        ecdsa384 = bytes.fromhex("300a06082a8648ce3d040303")
        ecdsa384_params = bytes.fromhex("300c06082a8648ce3d0403030500")
        for name, data in (("alg", alg), ("alg-bad", alg_no_null),
                           ("sha1", sha1), ("e384", ecdsa384),
                           ("e384p", ecdsa384_params)):
            lines += self._forth_bytes(f"tv-{name}", data)
        lines.extend([
            f'." AI=" tv-alg tv-alg {len(alg)} + X509-PARSE-ALG . .',
            f'." BI=" tv-alg-bad tv-alg-bad {len(alg_no_null)} + '
            "X509-PARSE-ALG . .",
            f'." SI=" tv-sha1 tv-sha1 {len(sha1)} + X509-PARSE-ALG . .',
            f'." EI=" tv-e384 tv-e384 {len(ecdsa384)} + X509-PARSE-ALG . .',
            f'." PI=" tv-e384p tv-e384p {len(ecdsa384_params)} + '
            "X509-PARSE-ALG . .",
        ])
        text = self._run_kdos(lines)
        for label in ("V", "A", "C", "D", "G", "H", "I"):
            self.assertIn(f"{label}=0 ", text, label)
        for label in ("N", "Z", "O", "B"):
            self.assertIn(f"{label}=-1 ", text, label)
        for label in ("W", "F", "J", "K"):
            self.assertIn(f"{label}=-2 ", text, label)
        self.assertIn("VKEY=1 256 3 ", text)
        self.assertIn("HKEY=512 ", text)
        self.assertIn("AI=0 1025 ", text)
        self.assertIn("BI=-1 0 ", text)
        self.assertIn("SI=0 0 ", text)
        self.assertIn("EI=0 1283 ", text)
        self.assertIn("PI=-1 0 ", text)

    def test_4096_anchor_scope_and_512_byte_signature_are_bounded(self):
        """A 2048-bit key with a 4096-bit issuer signature is borrowed safely."""
        root = self._fixture("rsa4096_root.der")
        intermediate = self._fixture("rsa2048_by_4096_intermediate.der")
        bundle = self._bundle(root)
        pinned_bundle = self._bundle(intermediate)
        lines = self._forth_bytes("tv-root4096", root)
        lines += self._forth_bytes("tv-int2048", intermediate)
        lines += self._forth_bytes("tv-bundle4096", bundle)
        lines += self._forth_bytes("tv-pinned2048", pinned_bundle)
        lines.extend([
            "CREATE tv-int-desc /X509-CERT ALLOT",
            f'tv-int2048 {len(intermediate)} tv-int-desc X509-DESC-PARSE '
            '." DESC=" .',
            '." PUB=" tv-int-desc XC.PUB-U + @ .',
            '." SIG=" tv-int-desc XC.SIG-U + @ .',
            f'tv-int2048 {len(intermediate)} X509-PARSE ." LEGACY=" .',
            f'tv-bundle4096 {len(bundle)} TLS-TRUST-LOAD ." TRUST=" .',
            '." COUNT=" TLS-TRUST-COUNT @ .',
            f'tv-pinned2048 {len(pinned_bundle)} TLS-TRUST-LOAD '
            '." PINNED=" .',
            '." PINCOUNT=" TLS-TRUST-COUNT @ .',
        ])
        text = self._run_kdos(lines)
        self.assertIn("DESC=0 ", text)
        self.assertIn("PUB=256 ", text)
        self.assertIn("SIG=512 ", text)
        self.assertIn("LEGACY=-1 ", text)
        self.assertIn("TRUST=0 ", text)
        self.assertIn("COUNT=1 ", text)
        self.assertIn("PINNED=0 ", text)
        self.assertIn("PINCOUNT=1 ", text)

    def test_pkcs1_padding_failure_axes(self):
        digest = hashlib.sha256(b"certificate body").digest()
        prefix = bytes.fromhex("3031300d060960864801650304020105000420")
        valid = b"\x00\x01" + b"\xff" * 202 + b"\x00" + prefix + digest
        variants = {
            "ok": valid,
            "ps": valid[:17] + b"\x7f" + valid[18:],
            "delim": valid[:204] + b"\x01" + valid[205:],
            "di": valid[:210] + bytes([valid[210] ^ 1]) + valid[211:],
            "hash": valid[:-1] + bytes([valid[-1] ^ 1]),
            "lead": b"\x01" + valid[1:],
            "bt": b"\x00\x02" + valid[2:],
        }
        lines = self._forth_bytes("tv-h", digest)
        for name, em in variants.items():
            lines += self._forth_bytes(f"tv-{name}", em)
            lines.append(
                f'." {name.upper()}=" tv-h 32 tv-{name} 256 _RSA-PKCS1-EM? .'
            )
        # SHA-384 and SHA-512 select their own DigestInfo.
        for hname, prefix_hex in (
            ("sha384", "3041300d060960864801650304020205000430"),
            ("sha512", "3051300d060960864801650304020305000440"),
        ):
            hd = hashlib.new(hname, b"certificate body").digest()
            em = (b"\x00\x01" + b"\xff" * (256 - 3 - 19 - len(hd)) + b"\x00"
                  + bytes.fromhex(prefix_hex) + hd)
            lines += self._forth_bytes(f"tv-h{hname}", hd)
            lines += self._forth_bytes(f"tv-em{hname}", em)
            lines.append(
                f'." {hname.upper()}=" tv-h{hname} {len(hd)} tv-em{hname} 256 '
                "_RSA-PKCS1-EM? ."
            )
        # Fewer than eight padding bytes, and an unknown digest length.
        short = b"\x00\x01" + b"\xff" * 7 + b"\x00" + prefix + digest
        lines += self._forth_bytes("tv-short", short)
        lines.extend([
            f'." SHORTPS=" tv-h 32 tv-short {len(short)} _RSA-PKCS1-EM? .',
            '." BADHASH=" tv-h 20 tv-ok 256 _RSA-PKCS1-EM? .',
        ])
        text = self._run_kdos(lines)
        self.assertIn("OK=0 ", text)
        self.assertIn("SHA384=0 ", text)
        self.assertIn("SHA512=0 ", text)
        for label in ("PS", "DELIM", "DI", "HASH", "LEAD", "BT", "SHORTPS",
                      "BADHASH"):
            self.assertIn(f"{label}=-1 ", text)

    def test_pss_padding_failure_axes_and_real_signature(self):
        transcript = b"rsa authenticated transcript"
        transcript_hash = hashlib.sha256(transcript).digest()
        content = (b" " * 64 + b"TLS 1.3, server CertificateVerify" +
                   b"\x00" + transcript_hash)
        message_hash = hashlib.sha256(content).digest()
        salt = bytes(range(32))
        valid = self._pss_em(message_hash, salt)
        variants = {
            "ok": valid,
            "high": bytes([valid[0] | 0x80]) + valid[1:],
            "trailer": valid[:-1] + b"\xbd",
            "pad": self._pss_em(message_hash, salt, pad_byte=1),
            "delim": self._pss_em(message_hash, salt, delimiter=0),
            "salt": self._pss_em(message_hash, salt, corrupt_salt=True),
        }
        leaf = self._fixture("rsa_leaf.der")
        modulus = self._rsa_modulus(leaf)
        signature = self._fixture("rsa_cv_sig")
        lines = self._forth_bytes("tv-mh", message_hash)
        for name, em in variants.items():
            lines += self._forth_bytes(f"tv-{name}", em)
            lines.append(
                f'." {name.upper()}=" tv-mh 32 tv-{name} 256 2048 '
                "_RSA-PSS-EM? ."
            )
        # The check unmasks em in place, so the stack-balance run gets a
        # fresh copy of the valid encoding.
        lines += self._forth_bytes("tv-ok-again", valid)
        lines.extend([
            "VARIABLE pss-em-depth  DEPTH pss-em-depth !",
            'tv-mh 32 tv-ok-again 256 2048 _RSA-PSS-EM? ." EM=" .',
            'DEPTH pss-em-depth @ = ." EM-BALANCED=" .',
        ])
        lines += self._key_lines("tv-key", modulus)
        lines += self._forth_bytes("tv-sig", signature)
        lines.extend([
            "VARIABLE pss-depth  DEPTH pss-depth !",
            '." REAL=" tv-mh 32 tv-sig 256 tv-key RSA-PSS-VERIFY .',
            'DEPTH pss-depth @ = ." PSS-BALANCED=" .',
        ])
        text = self._run_kdos(lines, max_steps=900_000_000)
        self.assertIn("OK=0 ", text)
        self.assertIn("EM=0 ", text)
        self.assertIn("EM-BALANCED=-1 ", text)
        self.assertIn("REAL=0 ", text)
        self.assertIn("PSS-BALANCED=-1 ", text)
        for label in ("HIGH", "TRAILER", "PAD", "DELIM", "SALT"):
            self.assertIn(f"{label}=-1 ", text)

    def test_rsa_anchor_validates_rsa_and_ecdsa_leaf_signatures(self):
        root = self._fixture("rsa_root.der")
        rsa_leaf = self._fixture("rsa_leaf.der")
        ec_leaf = self._fixture("ecdsa_by_rsa_leaf.der")
        bundle = self._bundle(root)
        lines = self._forth_bytes("tv-trust", bundle)
        lines += self._forth_bytes("tv-rsa-leaf", rsa_leaf)
        lines += self._forth_bytes("tv-ec-leaf", ec_leaf)
        lines.extend([
            "CREATE tv-chain /X509-CERT 2 * ALLOT",
            f'tv-trust {len(bundle)} TLS-TRUST-LOAD ." TRUST=" .',
            f'tv-rsa-leaf {len(rsa_leaf)} tv-chain X509-DESC-PARSE ." RP=" .',
            f'tv-ec-leaf {len(ec_leaf)} tv-chain /X509-CERT + '
            'X509-DESC-PARSE ." EP=" .',
            f'tv-chain 1 S" rsa.test.example" {self.NOW} '
            'X509-VERIFY-CHAIN ." RSA=" .',
            f'tv-chain /X509-CERT + 1 S" rsa.test.example" {self.NOW} '
            'X509-VERIFY-CHAIN ." MIXED=" .',
        ])
        text = self._run_kdos(lines, max_steps=1_200_000_000)
        for token in ("TRUST=0 ", "RP=0 ", "EP=0 ", "RSA=0 ", "MIXED=0 "):
            self.assertIn(token, text)

    def test_tls_certificate_verify_rsa_pss_dispatch(self):
        leaf = self._fixture("rsa_leaf.der")
        modulus = self._rsa_modulus(leaf)
        signature = self._fixture("rsa_cv_sig")
        transcript = b"rsa authenticated transcript"
        body = b"\x08\x04" + len(signature).to_bytes(2, "big") + signature
        message = b"\x0f" + len(body).to_bytes(3, "big") + body
        lines = self._forth_bytes("tv-mod", modulus)
        lines += self._forth_bytes("tv-cv", message)
        lines += self._forth_bytes("tv-tr", transcript)
        lines.extend([
            "tv-mod _TLS-SERVER-PUBKEY 256 CMOVE",
            "256 _TLS-SERVER-PUBKEY-LEN !",
            "X509-KEY-RSA _TLS-SERVER-PUBKEY-ALGO !",
            "RSA-E-65537 _TLS-SERVER-PUBKEY-E 3 CMOVE",
            "3 _TLS-SERVER-PUBKEY-E-U !",
            "VARIABLE tv-ctx 0 TLS-CTX@ tv-ctx !",
            "TLS-ROLE-CLIENT tv-ctx @ TLS-CTX.ROLE !",
            "TLS-HASH-SHA256 tv-ctx @ TLS-CTX.HASH-ID !",
            "TLS-SUITE-AES128-SHA256 tv-ctx @ TLS-CTX.SUITE !",
            "TLS-TR-RESET",
            f"tv-tr {len(transcript)} TLS-TR-APPEND",
            "VARIABLE cv-depth  DEPTH cv-depth !",
            f'tv-ctx @ tv-cv {len(message)} TLS-VERIFY-CERT-SIG ." CV=" .',
            'DEPTH cv-depth @ = ." CV-BALANCED=" .',
            "4 tv-cv 4 + C! 1 tv-cv 5 + C!",
            "VARIABLE badalg-depth  DEPTH badalg-depth !",
            f'tv-ctx @ tv-cv {len(message)} TLS-VERIFY-CERT-SIG ." BADALG=" .',
            'DEPTH badalg-depth @ = ." BADALG-BALANCED=" .',
        ])
        text = self._run_kdos(lines, max_steps=900_000_000)
        self.assertIn("CV=0 ", text)
        self.assertIn("CV-BALANCED=-1 ", text)
        self.assertIn("BADALG=-1 ", text)
        self.assertIn("BADALG-BALANCED=-1 ", text)


class TestKDOSX509Chain(_KDOSNetworkTestBase):
    """Native trust-bundle and bounded certificate-path validation."""

    FIXTURE_DIR = os.path.join(PROJECT_ROOT, "tests", "fixtures", "tls")
    NOW = 1_767_225_600  # 2026-01-01T00:00:00Z

    @classmethod
    def _fixture(cls, name: str) -> bytes:
        path = os.path.join(cls.FIXTURE_DIR, f"{name}.der.b64")
        with open(path, "rb") as stream:
            return base64.b64decode(stream.read())

    @staticmethod
    def _forth_bytes(name: str, data: bytes) -> list[str]:
        lines = [f"CREATE {name}"]
        for offset in range(0, len(data), 16):
            chunk = data[offset:offset + 16]
            lines.append(" ".join(f"{byte} C," for byte in chunk))
        return lines

    @staticmethod
    def _bundle(root: bytes, scope=b"test.example.com", flags=0,
                generation=1) -> bytes:
        return mpta_bundle(mpta_record(root, scope, flags=flags),
                           generation=generation)

    def _chain_lines(self, *, order=("leaf", "intermediate"),
                     scope=b"test.example.com") -> list[str]:
        root = self._fixture("root")
        bundle = self._bundle(root, scope=scope)
        lines = self._forth_bytes("tv-trust", bundle)
        certs = {name: self._fixture(name) for name in set(order)}
        for name in order:
            lines += self._forth_bytes(f"tv-{name}", certs[name])
        lines.extend([
            "CREATE tv-chain /X509-CERT 3 * ALLOT",
            f"tv-trust {len(bundle)} TLS-TRUST-LOAD",
            '." TRUST=" .',
        ])
        for index, name in enumerate(order):
            lines.extend([
                f"tv-{name} {len(certs[name])} "
                f"tv-chain /X509-CERT {index} * + X509-DESC-PARSE",
                f'." PARSE{index}=" .',
            ])
        return lines

    def test_chain_to_scoped_root(self):
        lines = self._chain_lines()
        lines.extend([
            f'tv-chain 2 S" test.example.com" {self.NOW} X509-VERIFY-CHAIN',
            '." CHAIN=" .',
            '." COUNT=" TLS-TRUST-COUNT @ .',
            '." VERSION=" TLS-TRUST-VERSION @ .',
            '." GENERATION=" TLS-TRUST-GENERATION @ .',
        ])
        text = self._run_kdos(lines, max_steps=5_000_000_000)
        self.assertIn("TRUST=0 ", text)
        self.assertIn("PARSE0=0 ", text)
        self.assertIn("PARSE1=0 ", text)
        self.assertIn("CHAIN=0 ", text)
        self.assertIn("COUNT=1 ", text)
        self.assertIn("VERSION=2 ", text)
        self.assertIn("GENERATION=1 ", text)

    def test_chain_rejects_hostname_time_signature_and_empty_trust(self):
        lines = self._chain_lines()
        lines.extend([
            f'tv-chain 2 S" wrong.example.com" {self.NOW} X509-VERIFY-CHAIN',
            '." HOST=" .',
            'tv-chain 2 S" test.example.com" 1700000000 X509-VERIFY-CHAIN',
            '." EARLY=" .',
            'tv-chain 2 S" test.example.com" 1900000000 X509-VERIFY-CHAIN',
            '." LATE=" .',
            "tv-chain XC.SIG-A + @ DUP C@ 1 XOR SWAP C!",
            f'tv-chain 2 S" test.example.com" {self.NOW} X509-VERIFY-CHAIN',
            '." SIG=" .',
            "TLS-TRUST-RESET",
            f'tv-chain 2 S" test.example.com" {self.NOW} X509-VERIFY-CHAIN',
            '." EMPTY=" .',
        ])
        text = self._run_kdos(lines, max_steps=8_000_000_000)
        self.assertIn("HOST=-4105 ", text)
        self.assertIn("EARLY=-4103 ", text)
        self.assertIn("LATE=-4104 ", text)
        self.assertIn("SIG=-4106 ", text)
        self.assertIn("EMPTY=-4102 ", text)

    def test_chain_accepts_extraneous_reordered_root(self):
        lines = self._chain_lines(order=("leaf", "root", "intermediate"))
        lines.extend([
            f'tv-chain 3 S" test.example.com" {self.NOW} X509-VERIFY-CHAIN',
            '." CHAIN=" .',
        ])
        text = self._run_kdos(lines, max_steps=7_000_000_000)
        self.assertIn("PARSE2=0 ", text)
        self.assertIn("CHAIN=0 ", text)

    def test_trust_scope_is_enforced(self):
        lines = self._chain_lines(scope=b"other.example")
        lines.extend([
            f'tv-chain 2 S" test.example.com" {self.NOW} X509-VERIFY-CHAIN',
            '." CHAIN=" .',
        ])
        text = self._run_kdos(lines, max_steps=5_000_000_000)
        self.assertIn("CHAIN=-4102 ", text)

    def test_dns_identity_and_scope_reject_malformed_labels(self):
        lines = self._chain_lines()
        bad_bundle = self._bundle(self._fixture("root"),
                                  scope=b"bad..example.com")
        lines += self._forth_bytes("tv-bad-trust", bad_bundle)
        lines.extend([
            f'tv-chain 2 S" test..example.com" {self.NOW} X509-VERIFY-CHAIN',
            '." HOST=" .',
            f"tv-bad-trust {len(bad_bundle)} TLS-TRUST-LOAD",
            '." SCOPE=" .',
        ])
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("HOST=-4105 ", text)
        self.assertIn("SCOPE=-4101 ", text)

    def test_trust_bundle_rejects_truncation(self):
        bundle = self._bundle(self._fixture("root"))
        lines = self._forth_bytes("tv-trust", bundle[:-1])
        lines.extend([
            f"tv-trust {len(bundle) - 1} TLS-TRUST-LOAD",
            '." TRUST=" .',
            '." COUNT=" TLS-TRUST-COUNT @ .',
        ])
        text = self._run_kdos(lines, max_steps=1_000_000_000)
        self.assertIn("TRUST=-4101 ", text)
        self.assertIn("COUNT=0 ", text)


# ---------------------------------------------------------------------------
#  TLS Certificate & CertificateVerify Handler tests — §16.7d
# ---------------------------------------------------------------------------


class TestKDOSTLSCertVerify(_KDOSNetworkTestBase):
    """Tests for §16.7d TLS certificate parsing and CertificateVerify
    verification wired into the handshake FSM."""

    NOW = TestKDOSX509Chain.NOW
    _CV_SIGNATURE = bytes.fromhex(
        "304402200cc1bd280427adf3879487204cfafe33"
        "04a18a547ed1c70661b28b01ba8658d802207e4"
        "82b4bd086cd5b1cf426ce19ae73e6e1e3a8e7e1"
        "532f53dc843d8679267646"
    )

    def _store_bytes(self, name, data: bytes) -> list[str]:
        return TestKDOSX509Chain._forth_bytes(name, data)

    @staticmethod
    def _fixture(name: str) -> bytes:
        return TestKDOSX509Chain._fixture(name)

    @classmethod
    def _set_epoch_lines(cls) -> list[str]:
        epoch_ms = cls.NOW * 1000
        rtc_epoch = 0xFFFF_FF00_0000_0B08
        return [
            f"{(epoch_ms >> (8 * index)) & 0xFF} "
            f"0x{rtc_epoch + index:016X} C!"
            for index in range(8)
        ]

    def _build_cert_msg(self, certs=None, context=b"",
                        entry_extensions=b"") -> bytes:
        """Build a bounded TLS 1.3 Certificate handshake message."""
        if certs is None:
            certs = [self._fixture("leaf"), self._fixture("intermediate")]
        entries = b"".join(
            len(cert).to_bytes(3, "big") + cert +
            len(entry_extensions).to_bytes(2, "big") + entry_extensions
            for cert in certs
        )
        list_data = len(entries).to_bytes(3, "big") + entries
        body = bytes([len(context)]) + context + list_data
        msg = bytes([11]) + len(body).to_bytes(3, 'big') + body
        return msg

    @classmethod
    def _build_cv_msg(cls) -> bytes:
        body = (
            (0x0403).to_bytes(2, "big") +
            len(cls._CV_SIGNATURE).to_bytes(2, "big") +
            cls._CV_SIGNATURE
        )
        return bytes([15]) + len(body).to_bytes(3, "big") + body

    def _trusted_lines(self, msg: bytes, host="test.example.com") -> list[str]:
        bundle = TestKDOSX509Chain._bundle(self._fixture("root"))
        lines = [PARSE_CERT_WORD]
        lines += self._store_bytes("tv-trust", bundle)
        lines += self._store_bytes("tv-msg", msg)
        lines += self._set_epoch_lines()
        lines.extend([
            f"tv-trust {len(bundle)} TLS-TRUST-LOAD",
            '." TRUST=" .',
        ])
        if host is None:
            lines.append("0 TLS-SNI-LEN !")
        else:
            lines.extend([
                f'S" {host}" TLS-SNI-HOST SWAP CMOVE',
                f"{len(host)} TLS-SNI-LEN !",
            ])
        return lines

    def test_tls_parse_certificate_success(self):
        """A complete trusted chain authenticates before exposing its key."""
        msg = self._build_cert_msg()
        lines = self._trusted_lines(msg)
        lines.extend([
            "VARIABLE cert-depth  DEPTH cert-depth !",
            f"tv-msg {len(msg)} tv-parse-cert",
            '." FLAG=" .',
            'DEPTH cert-depth @ = ." CERT-BALANCED=" .',
            '." LAST=" TLS-CERT-LAST-ERROR @ .',
            '." CERTS=" TLS-PEER-CERT-COUNT @ .',
            '." ALGO=" _TLS-SERVER-PUBKEY-ALGO @ .',
            '." PKLEN=" _TLS-SERVER-PUBKEY-LEN @ .',
        ])
        text = self._run_kdos(lines, max_steps=5_000_000_000)
        self.assertIn("TRUST=0 ", text)
        self.assertIn("FLAG=0 ", text)
        self.assertIn("CERT-BALANCED=-1 ", text)
        self.assertIn("LAST=0 ", text)
        self.assertIn("CERTS=2 ", text)
        self.assertIn("ALGO=2 ", text)      # X509-KEY-P256
        self.assertIn("PKLEN=65 ", text)

    def test_tls_parse_certificate_hostname_mismatch(self):
        """The authenticated path remains bound to the requested SNI."""
        msg = self._build_cert_msg()
        lines = self._trusted_lines(msg, host="evil.attacker.com")
        lines.extend([
            f"tv-msg {len(msg)} tv-parse-cert",
            '." FLAG=" .',
            '." PKLEN=" _TLS-SERVER-PUBKEY-LEN @ .',
        ])
        text = self._run_kdos(lines, max_steps=3_000_000_000)
        self.assertIn("FLAG=-4105 ", text)
        self.assertIn("PKLEN=0 ", text)

    def test_tls_parse_certificate_requires_sni(self):
        """Secure server authentication never silently skips hostname checks."""
        msg = self._build_cert_msg()
        lines = self._trusted_lines(msg, host=None)
        lines.extend([
            f"tv-msg {len(msg)} tv-parse-cert",
            '." FLAG=" .',
            '." PKLEN=" _TLS-SERVER-PUBKEY-LEN @ .',
        ])
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("FLAG=-4105 ", text)
        self.assertIn("PKLEN=0 ", text)

    def test_tls_parse_certificate_requires_trust(self):
        """A syntactically valid chain cannot publish a key without trust."""
        msg = self._build_cert_msg()
        lines = [PARSE_CERT_WORD]
        lines += self._store_bytes("tv-msg", msg) + self._set_epoch_lines()
        lines.extend([
            "TLS-TRUST-RESET",
            'S" test.example.com" TLS-SNI-HOST SWAP CMOVE',
            "16 TLS-SNI-LEN !",
            f"tv-msg {len(msg)} tv-parse-cert",
            '." FLAG=" .',
            '." PKLEN=" _TLS-SERVER-PUBKEY-LEN @ .',
        ])
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("FLAG=-4102 ", text)
        self.assertIn("PKLEN=0 ", text)

    def test_tls_parse_certificate_rejects_bad_framing(self):
        """Handshake and certificate-list lengths must be exact."""
        msg = bytearray(self._build_cert_msg())
        msg[3] ^= 1
        lines = self._trusted_lines(bytes(msg))
        lines.extend([
            f"tv-msg {len(msg)} tv-parse-cert",
            '." FLAG=" .',
        ])
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("FLAG=-4101 ", text)

    def test_tls_parse_certificate_bounds_entry_extensions(self):
        """Per-certificate TLS extensions are skipped only within bounds."""
        msg = self._build_cert_msg(entry_extensions=b"\x00\x00\x00\x00")
        lines = self._trusted_lines(msg)
        lines.extend([
            f"tv-msg {len(msg)} tv-parse-cert",
            '." FLAG=" .',
        ])
        text = self._run_kdos(lines, max_steps=5_000_000_000)
        self.assertIn("FLAG=0 ", text)

    def test_tls_parse_certificate_ignores_unusable_nonleaf_extra(self):
        """An unusable extra cannot block a valid explicitly anchored path."""
        p256_oid = bytes.fromhex("06082a8648ce3d030107")
        unknown_curve_oid = bytes.fromhex("06082a8648ce3d030108")
        extra = self._fixture("intermediate").replace(
            p256_oid, unknown_curve_oid, 1
        )
        self.assertNotEqual(extra, self._fixture("intermediate"))
        msg = self._build_cert_msg(certs=[
            self._fixture("leaf"),
            self._fixture("intermediate"),
            extra,
        ])
        lines = self._trusted_lines(msg)
        lines.extend([
            f"tv-msg {len(msg)} tv-parse-cert",
            '." FLAG=" .',
            '." CERTS=" TLS-PEER-CERT-COUNT @ .',
        ])
        text = self._run_kdos(lines, max_steps=5_000_000_000)
        self.assertIn("FLAG=0 ", text)
        self.assertIn("CERTS=2 ", text)

    def test_certificate_verify_opens_peer_auth_gate(self):
        """Only proof of the trusted leaf key sets the connection auth bit."""
        cert_msg = self._build_cert_msg()
        cv_msg = self._build_cv_msg()
        transcript = b"authenticated transcript"
        lines = self._trusted_lines(cert_msg)
        lines += self._store_bytes("tv-transcript", transcript)
        lines += self._store_bytes("tv-cv", cv_msg)
        lines.extend([
            f"tv-msg {len(cert_msg)} tv-parse-cert",
            '." CERT=" .',
            "TLS-TR-RESET",
            f"tv-transcript {len(transcript)} TLS-TR-APPEND",
            "VARIABLE tv-ctx 0 TLS-CTX@ tv-ctx !",
            "TLS-ROLE-CLIENT tv-ctx @ TLS-CTX.ROLE !",
            "TLS-HASH-SHA256 tv-ctx @ TLS-CTX.HASH-ID !",
            "TLS-SUITE-AES128-SHA256 tv-ctx @ TLS-CTX.SUITE !",
            "TLSS-HANDSHAKE tv-ctx @ TLS-CTX.STATE !",
            "TLSH-CERT-RCVD tv-ctx @ TLS-CTX.HS-STATE !",
            f"tv-ctx @ tv-cv {len(cv_msg)} TLS-PROCESS-HS-MSG",
            '." CV=" .',
            '." AUTH=" tv-ctx @ TLS-CTX.PEER-AUTH @ .',
            '." STATE=" tv-ctx @ TLS-CTX.HS-STATE @ .',
            "tv-cv 12 + DUP C@ 1 XOR SWAP C!",
            f"tv-ctx @ tv-cv {len(cv_msg)} TLS-VERIFY-CERT-SIG",
            '." CORRUPT=" .',
        ])
        text = self._run_kdos(lines, max_steps=8_000_000_000)
        self.assertIn("CERT=0 ", text)
        self.assertIn("CV=0 ", text)
        self.assertIn("AUTH=1 ", text)
        self.assertIn("STATE=5 ", text)
        self.assertIn("CORRUPT=-1 ", text)


# ---------------------------------------------------------------------------
#  TLS 1.3 Application Data tests — §16.10 / §16.11
# ---------------------------------------------------------------------------


class TestKDOSSignatureVectors(_KDOSNetworkTestBase):
    """RSA at 2048, 3072 and 4096 bits and ECDSA P-384 against signatures
    made by an independent implementation (tests/fixtures/tls/
    signature_vectors.json)."""

    VECTORS = os.path.join(
        PROJECT_ROOT, "tests", "fixtures", "tls", "signature_vectors.json"
    )

    @classmethod
    def _vectors(cls) -> dict:
        with open(cls.VECTORS, encoding="utf-8") as stream:
            return json.load(stream)

    _forth_bytes = staticmethod(TestKDOSRSA._forth_bytes)
    _key_lines = classmethod(TestKDOSRSA._key_lines.__func__)

    def test_rsa_pkcs1_and_pss_at_every_width(self):
        lines = []
        tokens = []
        for key_index, key in enumerate(self._vectors()["rsa"]):
            modulus = bytes.fromhex(key["modulus"])
            name = f"k{key_index}"
            lines += self._key_lines(name, modulus)
            for sig_index, entry in enumerate(key["signatures"]):
                tag = f"{name}s{sig_index}"
                digest = bytes.fromhex(entry["digest"])
                signature = bytes.fromhex(entry["signature"])
                word = ("RSA-PKCS1-VERIFY" if entry["scheme"] == "pkcs1"
                        else "RSA-PSS-VERIFY")
                lines += self._forth_bytes(f"{tag}-h", digest)
                lines += self._forth_bytes(f"{tag}-s", signature)
                lines.extend([
                    f'." V{tag}=" {tag}-h {len(digest)} {tag}-s '
                    f"{len(signature)} {name} {word} .",
                    f"{tag}-h 3 + DUP C@ 1 XOR SWAP C!",
                    f'." H{tag}=" {tag}-h {len(digest)} {tag}-s '
                    f"{len(signature)} {name} {word} .",
                    f"{tag}-h 3 + DUP C@ 1 XOR SWAP C!",
                    f"{tag}-s 40 + DUP C@ 1 XOR SWAP C!",
                    f'." S{tag}=" {tag}-h {len(digest)} {tag}-s '
                    f"{len(signature)} {name} {word} .",
                ])
                tokens += [f"V{tag}=0 ", f"H{tag}=-1 ", f"S{tag}=-1 "]
        text = self._run_kdos(lines, max_steps=3_000_000_000)
        missing = [token for token in tokens if token not in text]
        self.assertEqual(missing, [])

    def test_p384_signatures(self):
        vectors = self._vectors()["p384"]
        lines = []
        tokens = []
        for index, entry in enumerate(vectors):
            tag = f"e{index}"
            public = bytes.fromhex(entry["public"])
            digest = bytes.fromhex(entry["digest"])
            signature = bytes.fromhex(entry["signature"])
            lines += self._forth_bytes(f"{tag}-q", public)
            lines += self._forth_bytes(f"{tag}-h", digest)
            lines += self._forth_bytes(f"{tag}-s", signature)
            call = (f"{tag}-h {len(digest)} {tag}-q {tag}-s {len(signature)} "
                    "ECDSA-P384-VERIFY")
            lines.extend([
                f'." V{tag}=" {call} .',
                f"{tag}-h 7 + DUP C@ 1 XOR SWAP C!",
                f'." H{tag}=" {call} .',
                f"{tag}-h 7 + DUP C@ 1 XOR SWAP C!",
            ])
            tokens += [f"V{tag}=0 ", f"H{tag}=-1 "]
        # Malformed inputs, against the first vector.
        first = vectors[0]
        public = bytearray(bytes.fromhex(first["public"]))
        digest = bytes.fromhex(first["digest"])
        signature = bytes.fromhex(first["signature"])
        off_curve = bytearray(public)
        off_curve[-1] ^= 1
        n = int("ffffffffffffffffffffffffffffffffffffffffffffffffc7634d81"
                "f4372ddf581a0db248b0a77aecec196accc52973", 16)
        r_bytes = n.to_bytes(49, "big")       # s = n, with its sign byte
        s_equal_n = bytes([0x30, 3 + 2 + len(r_bytes)]) + \
            bytes([0x02, 1, 1]) + bytes([0x02, len(r_bytes)]) + r_bytes
        zero_r = bytes([0x30, 6, 0x02, 1, 0, 0x02, 1, 1])
        lines += self._forth_bytes("m-q", bytes(public))
        lines += self._forth_bytes("m-off", bytes(off_curve))
        lines += self._forth_bytes("m-h", digest)
        lines += self._forth_bytes("m-s", signature)
        lines += self._forth_bytes("m-sn", s_equal_n)
        lines += self._forth_bytes("m-r0", zero_r)
        lines += self._forth_bytes("m-trail", signature + b"\x00")
        lines.extend([
            f'." OFF=" m-h 48 m-off m-s {len(signature)} ECDSA-P384-VERIFY .',
            f'." SN=" m-h 48 m-q m-sn {len(s_equal_n)} ECDSA-P384-VERIFY .',
            f'." R0=" m-h 48 m-q m-r0 {len(zero_r)} ECDSA-P384-VERIFY .',
            f'." TRAIL=" m-h 48 m-q m-trail {len(signature) + 1} '
            "ECDSA-P384-VERIFY .",
            "2 m-q C!",
            f'." FORM=" m-h 48 m-q m-s {len(signature)} ECDSA-P384-VERIFY .',
            "VARIABLE p384-depth DEPTH p384-depth !",
            f"m-h 48 m-off m-s {len(signature)} ECDSA-P384-VERIFY DROP",
            'DEPTH p384-depth @ = ." BALANCED=" .',
        ])
        tokens += ["OFF=-1 ", "SN=-1 ", "R0=-1 ", "TRAIL=-1 ", "FORM=-1 ",
                   "BALANCED=-1 "]
        text = self._run_kdos(lines, max_steps=3_000_000_000)
        missing = [token for token in tokens if token not in text]
        self.assertEqual(missing, [])


class TestKDOSSignatureCost(_KDOSNetworkTestBase):
    """Device cycles of one signature check at each width and curve.

    The bounds sit well above the measured costs so a large regression
    fails while ordinary code movement does not; the measured values print
    for the record (pytest -s)."""

    _forth_bytes = staticmethod(TestKDOSRSA._forth_bytes)
    _key_lines = classmethod(TestKDOSRSA._key_lines.__func__)

    # Measured 2026-11-24 on the emulator, in cycles, with each bound.
    BOUNDS = {
        "MM2": 12_000,
        "MM8": 110_000,
        "MM16": 430_000,
        "RSA2048": 3_400_000,
        "RSA3072": 7_000_000,
        "RSA4096": 12_000_000,
        "PSS2048": 3_500_000,
        "P384": 140_000_000,
        "P256": 3_600_000,
    }

    def test_signature_check_cycles(self):
        vectors = TestKDOSSignatureVectors._vectors()
        lines = []
        for digits in (2, 8, 16):
            n = (1 << (256 * digits)) - 159 if digits > 2 else (
                (1 << 384) - (1 << 128) - (1 << 96) + (1 << 32) - 1)
            a = (n // 3).to_bytes(32 * digits, "little")
            lines += self._forth_bytes(f"c-n{digits}", n.to_bytes(32 * digits, "little"))
            lines += self._forth_bytes(f"c-a{digits}", a)
            lines.extend([
                f"CREATE c-ctx{digits} {digits} MPM-CONTEXT-SIZE ALLOT",
                f"CREATE c-o{digits} {32 * digits} ALLOT",
                f"c-n{digits} {digits} c-ctx{digits} MPM-INIT DROP",
                f": c-mm{digits} PERF-CYCLES c-a{digits} c-a{digits} c-o{digits} "
                f"c-ctx{digits} MPM-MUL PERF-CYCLES SWAP - ;",
                f'." CYCLES MM{digits}=" c-mm{digits} .',
            ])
        for key in vectors["rsa"]:
            bits = key["bits"]
            pkcs1 = next(e for e in key["signatures"]
                         if e["scheme"] == "pkcs1" and e["hash"] == "sha256")
            lines += self._key_lines(f"c-k{bits}", bytes.fromhex(key["modulus"]))
            lines += self._forth_bytes(f"c-h{bits}", bytes.fromhex(pkcs1["digest"]))
            lines += self._forth_bytes(f"c-s{bits}", bytes.fromhex(pkcs1["signature"]))
            lines.extend([
                f": c-rsa{bits} PERF-CYCLES >R c-h{bits} 32 c-s{bits} "
                f"{bits // 8} c-k{bits} RSA-PKCS1-VERIFY PERF-CYCLES R> - ;",
                f'." CYCLES RSA{bits}=" c-rsa{bits} . ." OK=" .',
            ])
            if bits == 2048:
                pss = next(e for e in key["signatures"]
                           if e["scheme"] == "pss" and e["hash"] == "sha256")
                lines += self._forth_bytes("c-ph", bytes.fromhex(pss["digest"]))
                lines += self._forth_bytes("c-ps", bytes.fromhex(pss["signature"]))
                lines.extend([
                    ": c-pss PERF-CYCLES >R c-ph 32 c-ps 256 c-k2048 "
                    "RSA-PSS-VERIFY PERF-CYCLES R> - ;",
                    '." CYCLES PSS2048=" c-pss . ." OK=" .',
                ])
        p384 = vectors["p384"][0]
        lines += self._forth_bytes("c-eq", bytes.fromhex(p384["public"]))
        lines += self._forth_bytes("c-eh", bytes.fromhex(p384["digest"]))
        signature = bytes.fromhex(p384["signature"])
        lines += self._forth_bytes("c-es", signature)
        lines.extend([
            f": c-p384 PERF-CYCLES >R c-eh 48 c-eq c-es {len(signature)} "
            "ECDSA-P384-VERIFY PERF-CYCLES R> - ;",
            '." CYCLES P384=" c-p384 . ." OK=" .',
        ])
        digest = bytes.fromhex(
            "b213ca91f5dcd3d45fbe66562ab30eaf2dc9e89227656de3bd29b389f4e88530"
        )
        p256_sig = bytes.fromhex(
            "304502207723d84e331d96bed730b21ceef0c1434136e0535fc9aa6ae556628e"
            "f7e3fe6c0221009f51dd3da53c8f818caf4667f84cf5ef207be5c25792e9fa31"
            "005410ad692686"
        )
        lines += self._forth_bytes("c-qh", digest)
        lines += self._forth_bytes("c-qq", TestKDOSX509._PUB_KEY)
        lines += self._forth_bytes("c-qs", p256_sig)
        lines.extend([
            f": c-p256 PERF-CYCLES >R c-qh c-qq c-qs {len(p256_sig)} "
            "ECDSA-P256-VERIFY PERF-CYCLES R> - ;",
            '." CYCLES P256=" c-p256 . ." OK=" .',
        ])
        text = self._run_kdos(lines, max_steps=3_000_000_000)
        measured = {
            match.group(1): int(match.group(2))
            for match in re.finditer(r"CYCLES (\w+)=(\d+) ", text)
        }
        for name, cycles in sorted(measured.items()):
            print(f"{name}: {cycles:,} cycles, {cycles / 1e8:.4f} s at 100 MHz")
        self.assertEqual(sorted(measured), sorted(self.BOUNDS))
        self.assertNotIn("OK=-", text)
        for name, bound in self.BOUNDS.items():
            self.assertLess(measured[name], bound, name)


if __name__ == "__main__":
    unittest.main()
