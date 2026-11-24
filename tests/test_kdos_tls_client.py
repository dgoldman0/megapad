"""KDOS TLS 1.3 client handshake: ClientHello construction and its exact
wire layout, generic ALPN offers and selection, ServerHello parsing, the key
schedule, Finished, the client handshake state machine, and the exporter
(RFC 8446 section 7.5) whose tests build on the key schedule's setup.

These classes run on the KDOS + networking snapshot that tests/test_system.py
builds, as their own unit, so a change to the client handshake can be
checked without the whole system test.
"""

import random
import unittest

from tests.test_system import (  # noqa: F401  (the harness the classes share)
    _KDOSNetworkTestBase,
)


# ---------------------------------------------------------------------------
#  TLS generic ALPN tests — protocol-neutral offer and exact selection
# ---------------------------------------------------------------------------

class TestKDOSTLSGenericALPN(_KDOSNetworkTestBase):
    """ALPN helpers operate on exact caller bytes without key generation."""

    def test_generic_alpn_exact_offer_and_selection(self):
        """A non-HTTP ProtocolName is copied, encoded, and selected exactly."""
        lines = [
            "VARIABLE test-ctx 0 TLS-CTX@ test-ctx !",
            "test-ctx @ /TLS-CTX 0 FILL",
            'test-ctx @ S" rabbit/1" TLS-ALPN-CONFIGURE ." CFG=" .',
            "CREATE offer 32 ALLOT offer 32 165 FILL",
            "VARIABLE alpn-sp SP@ alpn-sp !",
            "test-ctx @ offer 32 TLS-ALPN-BUILD-OFFER",
            '." BIOR=" . ." BLEN=" .',
            '." BALANCED=" SP@ alpn-sp @ = .',
            '." TYPE=" offer NW16@ .',
            '." ELEN=" offer 2 + NW16@ .',
            '." LLEN=" offer 4 + NW16@ .',
            '." NLEN=" offer 6 + C@ .',
            '." OFFER=" offer 7 + 8 TYPE CR',
            '." MAX-WIRE=" 927 TLS-SNI-HOST-CAPACITY 9 + + '
            'TLS-ALPN-NAME-MAX 7 + + 77 + 9 + .',
            '." CAP=" TLS-CH-BUF-CAPACITY .',
            "test-ctx @ offer 4 + 11 TLS-ALPN-ACCEPT-SELECTION",
            '." SIOR=" .',
            '." EARLY=" test-ctx @ TLS-ALPN-SELECTED NIP .',
            "TLSS-ESTABLISHED test-ctx @ TLS-CTX.STATE !",
            "1 test-ctx @ TLS-CTX.PEER-AUTH !",
            '." SELECTED=" test-ctx @ TLS-ALPN-SELECTED TYPE CR',
        ]
        text = self._run_kdos(lines)
        self.assertIn("CFG=0 ", text)
        self.assertIn("BIOR=0 ", text)
        self.assertIn("BLEN=15 ", text)
        self.assertIn("BALANCED=-1 ", text)
        self.assertIn("TYPE=16 ", text)
        self.assertIn("ELEN=11 ", text)
        self.assertIn("LLEN=9 ", text)
        self.assertIn("NLEN=8 ", text)
        self.assertIn("OFFER=rabbit/1", text)
        self.assertIn("MAX-WIRE=1537 ", text)
        self.assertIn("CAP=1537 ", text)
        self.assertIn("SIOR=0 ", text)
        self.assertIn("EARLY=0 ", text)
        self.assertIn("SELECTED=rabbit/1", text)

    def test_generic_alpn_bounds_duplicates_and_atomic_refusal(self):
        """Bounds and malformed EE input fail without partial publication."""
        lines = [
            "VARIABLE test-ctx 0 TLS-CTX@ test-ctx !",
            "test-ctx @ /TLS-CTX 0 FILL",
            'test-ctx @ S" rabbit/1" TLS-ALPN-CONFIGURE DROP',
            "CREATE offer 32 ALLOT offer 32 165 FILL",
            "test-ctx @ offer 32 TLS-ALPN-BUILD-OFFER 2DROP",
            "test-ctx @ offer 4 + 11 TLS-ALPN-ACCEPT-SELECTION DROP",
            "CREATE short-out 15 ALLOT short-out 15 90 FILL",
            "test-ctx @ short-out 14 TLS-ALPN-BUILD-OFFER",
            '." SHORT-IOR=" . ." SHORT-LEN=" .',
            '." SHORT-FIRST=" short-out C@ .',
            '." SHORT-LAST=" short-out 14 + C@ .',
            "test-ctx @ TLS-CTX.ALPN-NAME 32 TLS-ALPN-BUILD-OFFER",
            '." ALIAS-IOR=" . ." ALIAS-LEN=" .',
            '." ALIAS-CONFIG=" test-ctx @ TLS-ALPN-CONFIGURED TYPE CR',
            "CREATE max-name TLS-ALPN-NAME-MAX ALLOT",
            "max-name TLS-ALPN-NAME-MAX 113 FILL",
            "test-ctx @ max-name TLS-ALPN-NAME-MAX TLS-ALPN-CONFIGURE DROP",
            "CREATE max-offer 272 ALLOT",
            "test-ctx @ max-offer 272 TLS-ALPN-BUILD-OFFER",
            '." MAX-IOR=" . ." MAX-LEN=" .',
            '." MAX-ELEN=" max-offer 2 + NW16@ .',
            '." MAX-LLEN=" max-offer 4 + NW16@ .',
            '." MAX-NLEN=" max-offer 6 + C@ .',
            "test-ctx @ max-offer 4 + 258 TLS-ALPN-ACCEPT-SELECTION",
            '." MAX-SELECT=" .',
            'test-ctx @ S" rabbit/1" TLS-ALPN-CONFIGURE DROP',
            "test-ctx @ offer 4 + 11 TLS-ALPN-ACCEPT-SELECTION DROP",
            "test-ctx @ offer 4 + 10 TLS-ALPN-ACCEPT-SELECTION",
            '." TRUNC=" .',
            "test-ctx @ offer 4 + 12 TLS-ALPN-ACCEPT-SELECTION",
            '." TRAIL=" .',
            "test-ctx @ 0 1 TLS-ALPN-CONFIGURE",
            '." NULL=" .',
            "test-ctx @ offer 256 TLS-ALPN-CONFIGURE",
            '." LONG=" .',
            "test-ctx @ -4 8 TLS-ALPN-CONFIGURE",
            '." WRAP=" .',
            '." CONFIG=" test-ctx @ TLS-ALPN-CONFIGURED TYPE CR',
            "120 offer 7 + C!",
            "test-ctx @ offer 4 + 11 TLS-ALPN-ACCEPT-SELECTION",
            '." MISMATCH=" .',
            '." PRESERVED=" test-ctx @ TLS-CTX.ALPN-SELECTED-LEN @ .',
            "114 offer 7 + C!",
            "CREATE dup-ee 36 ALLOT dup-ee 36 0 FILL",
            "8 dup-ee C! 32 dup-ee 3 + C! 30 dup-ee 4 + NW16!",
            "offer dup-ee 6 + 15 MOVE offer dup-ee 21 + 15 MOVE",
            "test-ctx @ dup-ee 36 TLS-PARSE-ENCRYPTED-EXT",
            '." DUP=" .',
            '." AFTER-DUP=" test-ctx @ TLS-CTX.ALPN-SELECTED-LEN @ .',
            "TLSS-HANDSHAKE test-ctx @ TLS-CTX.STATE !",
            'test-ctx @ S" h2" TLS-ALPN-CONFIGURE',
            '." STARTED=" .',
            "TLSS-NONE test-ctx @ TLS-CTX.STATE !",
            "test-ctx @ 0 0 TLS-ALPN-CONFIGURE",
            '." CLEAR=" .',
            "test-ctx @ TLS-ALPN-CONFIGURED NIP",
            '." CLEAR-LEN=" .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("SHORT-IOR=-4205 ", text)
        self.assertIn("SHORT-LEN=0 ", text)
        self.assertIn("SHORT-FIRST=90 ", text)
        self.assertIn("SHORT-LAST=90 ", text)
        self.assertIn("ALIAS-IOR=-4205 ", text)
        self.assertIn("ALIAS-LEN=0 ", text)
        self.assertIn("ALIAS-CONFIG=rabbit/1", text)
        self.assertIn("MAX-IOR=0 ", text)
        self.assertIn("MAX-LEN=262 ", text)
        self.assertIn("MAX-ELEN=258 ", text)
        self.assertIn("MAX-LLEN=256 ", text)
        self.assertIn("MAX-NLEN=255 ", text)
        self.assertIn("MAX-SELECT=0 ", text)
        self.assertIn("TRUNC=-4205 ", text)
        self.assertIn("TRAIL=-4205 ", text)
        self.assertIn("NULL=-4205 ", text)
        self.assertIn("LONG=-4205 ", text)
        self.assertIn("WRAP=-4205 ", text)
        self.assertIn("CONFIG=rabbit/1", text)
        self.assertIn("MISMATCH=-4205 ", text)
        self.assertIn("PRESERVED=8 ", text)
        self.assertIn("DUP=-1 ", text)
        self.assertIn("AFTER-DUP=0 ", text)
        self.assertIn("STARTED=-4205 ", text)
        self.assertIn("CLEAR=0 ", text)
        self.assertIn("CLEAR-LEN=0 ", text)


# ---------------------------------------------------------------------------
#  TLS 1.3 Handshake tests — §16.9
# ---------------------------------------------------------------------------

class TestKDOSTLSHandshake(_KDOSNetworkTestBase):
    """Tests for §16.9 TLS 1.3 handshake — key schedule, ClientHello,
    ServerHello, Finished MAC, and handshake state machine."""

    # Python reference: HMAC-SHA3-256 / HKDF with SHA3-256
    # shared_secret = 0xAA * 32, transcript = 0xBB*32 || 0xCC*32
    # C_HS_KEY: [245, 102, 194, 81]  S_HS_KEY: [127, 190, 91, 227]
    # C_HS_IV:  [77, 146, 230, 138]  S_HS_IV:  [11, 205, 119, 228]

    _TLS_KS_SETUP = [
        "VARIABLE test-ctx  0 TLS-CTX@ test-ctx !",
        "TLS-ROLE-CLIENT test-ctx @ TLS-CTX.ROLE !",
        "TLS-HASH-SHA3-256 test-ctx @ TLS-CTX.HASH-ID !",
        "TLS-SUITE-X25519-SHA3 test-ctx @ TLS-CTX.SUITE !",
        # Shared secret = 32 bytes of 0xAA
        ": init-shared 32 0 DO 170 test-ctx @ TLS-CTX.SHARED I + C! LOOP ;",
        "init-shared",
        # Transcript = 0xBB*32 || 0xCC*32 (fake CH||SH, 64 bytes)
        "TLS-TR-RESET",
        "CREATE fake-ch 32 ALLOT  fake-ch 32 187 FILL",
        "CREATE fake-sh 32 ALLOT  fake-sh 32 204 FILL",
        "fake-ch 32 TLS-TR-APPEND",
        "fake-sh 32 TLS-TR-APPEND",
    ]

    def test_tls_crypto_dispatch_guards_restore_or_retain_owner_depth(self):
        """Ordinary throws unwind lock 10; failed lower cleanup retains it."""
        lines = [
            ": gd-zero? ( a u -- flag )",
            "  0 DO DUP I + C@ IF DROP 0 UNLOOP EXIT THEN LOOP",
            "  DROP -1 ;",
            ": gd-negative ( -- status ) -776 THROW ;",
            ": gd-tcd-negative ( -- )",
            "  ['] gd-negative _TCD-GUARD DROP ;",
            ": gd-tel-negative ( -- )",
            "  ['] gd-negative _TEL-GUARD DROP ;",
            "111 _TCD-CTX ! 222 _TCD-A ! 333 _TCD-CU !",
            "TLS-OWNER-TRY DROP",
            '." TCD-THROW=" \' gd-tcd-negative CATCH .',
            '." TCD-DEPTH=" TLS-OWNER-DEPTH @ .',
            '." TCD-WIPED=" _TCD-CTX @ _TCD-A @ OR _TCD-CU @ OR 0= .',
            "TLS-HKDF-LABEL TLS-HKDF-LABEL-CAPACITY 165 FILL",
            "111 _TEL-TLS-CTX ! 222 _TEL-SECRET ! 333 _TEL-OUT !",
            "TLS-OWNER-TRY DROP",
            '." TEL-THROW=" \' gd-tel-negative CATCH .',
            '." TEL-DEPTH=" TLS-OWNER-DEPTH @ .',
            '." TEL-META=" _TEL-TLS-CTX @ _TEL-SECRET @ OR '
            '_TEL-OUT @ OR 0= .',
            '." TEL-LABEL=" TLS-HKDF-LABEL '
            'TLS-HKDF-LABEL-CAPACITY gd-zero? .',
            ": gd-lower-throw ( a b c d e -- status )",
            "  2DROP 2DROP DROP SHA256-INIT DROP -779 THROW ;",
            ": gd-failing-clear ( -- status ) CRYPTO-HARDWARE ;",
            ": gd-positive-core ( -- status )",
            "  _HMAC-HKDF-TRY IF CRYPTO-STATE EXIT THEN",
            "  1 2 3 4 5 ['] gd-lower-throw",
            "  ['] _HKDF256-WIPE ['] gd-failing-clear",
            "  _HMAC-HKDF-GUARD ;",
            ": gd-positive-run ( -- )",
            "  ['] gd-positive-core _TCD-GUARD DROP ;",
            "TLS-OWNER-TRY DROP",
            '." POSITIVE=" \' gd-positive-run CATCH .',
            '." RETAINED=" TLS-OWNER-DEPTH @ .',
            '." OWNER=" _TLS-OWNER? .',
            '." LOWER-CLEAR=" SHA256-CLEAR .',
            "_HMAC-HKDF-RELEASE TLS-OWNER-RELEASE",
            '." RELEASED=" TLS-OWNER-DEPTH @ .',
            '." REUSE=" SHA256-INIT . SHA256-CLEAR DROP',
            '." GUARD-FINAL-DEPTH=" DEPTH .',
        ]
        text = self._run_kdos(lines)
        for token in (
            "TCD-THROW=-776 ", "TCD-DEPTH=0 ", "TCD-WIPED=-1 ",
            "TEL-THROW=-776 ", "TEL-DEPTH=0 ", "TEL-META=-1 ",
            "TEL-LABEL=-1 ", "POSITIVE=6 ", "RETAINED=1 ",
            "OWNER=-1 ", "LOWER-CLEAR=0 ", "RELEASED=0 ",
            "REUSE=0 ", "GUARD-FINAL-DEPTH=0 ",
        ):
            self.assertIn(token, text)

    def test_empty_hash_constant(self):
        """TLS-EMPTY-HASH contains SHA3-256 of empty string."""
        text = self._run_kdos(self._TLS_KS_SETUP + [
            '." H0=" test-ctx @ TLS-EMPTY-HASH C@ .',
            '." H1=" test-ctx @ TLS-EMPTY-HASH 1 + C@ .',
            '." H2=" test-ctx @ TLS-EMPTY-HASH 2 + C@ .',
            '." H3=" test-ctx @ TLS-EMPTY-HASH 3 + C@ .',
        ])
        self.assertIn("H0=167 ", text)   # SHA3-256("") = a7ffc6f8...
        self.assertIn("H1=255 ", text)
        self.assertIn("H2=198 ", text)
        self.assertIn("H3=248 ", text)

    def test_expand_label_key(self):
        """TLS-EXPAND-LABEL with 'key' label produces correct output."""
        lines = [
            "VARIABLE test-ctx 0 TLS-CTX@ test-ctx !",
            "TLS-ROLE-CLIENT test-ctx @ TLS-CTX.ROLE !",
            "TLS-HASH-SHA3-256 test-ctx @ TLS-CTX.HASH-ID !",
            "TLS-SUITE-X25519-SHA3 test-ctx @ TLS-CTX.SUITE !",
            "CREATE el-prk 32 ALLOT",
            ": init-prk 32 0 DO I el-prk I + C! LOOP ; init-prk",
            "CREATE el-out 32 ALLOT",
            "test-ctx @ el-prk TLS-L-KEY /TLS-L-KEY 0 0 32 el-out"
            " TLS-EXPAND-LABEL DROP",
            '." K0=" el-out C@ .',
            '." K1=" el-out 1 + C@ .',
            '." K2=" el-out 2 + C@ .',
            '." K3=" el-out 3 + C@ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("K0=144 ", text)
        self.assertIn("K1=218 ", text)
        self.assertIn("K2=9 ", text)
        self.assertIn("K3=27 ", text)

    def test_expand_label_iv(self):
        """TLS-EXPAND-LABEL with 'iv' label produces 12-byte output."""
        lines = [
            "VARIABLE test-ctx 0 TLS-CTX@ test-ctx !",
            "TLS-ROLE-CLIENT test-ctx @ TLS-CTX.ROLE !",
            "TLS-HASH-SHA3-256 test-ctx @ TLS-CTX.HASH-ID !",
            "TLS-SUITE-X25519-SHA3 test-ctx @ TLS-CTX.SUITE !",
            "CREATE el-prk 32 ALLOT",
            ": init-prk 32 0 DO I el-prk I + C! LOOP ; init-prk",
            "CREATE el-out 12 ALLOT",
            "test-ctx @ el-prk TLS-L-IV /TLS-L-IV 0 0 12 el-out"
            " TLS-EXPAND-LABEL DROP",
            '." V0=" el-out C@ .',
            '." V1=" el-out 1 + C@ .',
            '." V2=" el-out 2 + C@ .',
            '." V3=" el-out 3 + C@ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("V0=170 ", text)
        self.assertIn("V1=25 ", text)
        self.assertIn("V2=44 ", text)
        self.assertIn("V3=204 ", text)

    def test_transcript_reset_append(self):
        """TLS-TR-RESET/TLS-TR-APPEND manage transcript buffer."""
        lines = [
            "TLS-TR-RESET",
            '." L0=" TLS-HS-TR-LEN @ .',
            "CREATE tr-data 8 ALLOT  tr-data 8 65 FILL",
            "tr-data 8 TLS-TR-APPEND",
            '." L1=" TLS-HS-TR-LEN @ .',
            '." B0=" TLS-HS-TRANSCRIPT C@ .',
            "TLS-TR-RESET",
            '." L2=" TLS-HS-TR-LEN @ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("L0=0 ", text)
        self.assertIn("L1=8 ", text)
        self.assertIn("B0=65 ", text)    # 'A'
        self.assertIn("L2=0 ", text)

    def test_ks_handshake_wr_key(self):
        """TLS-KS-HANDSHAKE derives correct client HS key (WR-KEY)."""
        lines = self._TLS_KS_SETUP + [
            "test-ctx @ TLS-KS-HANDSHAKE DROP",
            '." W0=" test-ctx @ TLS-CTX.WR-KEY C@ .',
            '." W1=" test-ctx @ TLS-CTX.WR-KEY 1 + C@ .',
            '." W2=" test-ctx @ TLS-CTX.WR-KEY 2 + C@ .',
            '." W3=" test-ctx @ TLS-CTX.WR-KEY 3 + C@ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("W0=245 ", text)
        self.assertIn("W1=102 ", text)
        self.assertIn("W2=194 ", text)
        self.assertIn("W3=81 ", text)

    def test_ks_handshake_rd_key(self):
        """TLS-KS-HANDSHAKE derives correct server HS key (RD-KEY)."""
        lines = self._TLS_KS_SETUP + [
            "test-ctx @ TLS-KS-HANDSHAKE DROP",
            '." R0=" test-ctx @ TLS-CTX.RD-KEY C@ .',
            '." R1=" test-ctx @ TLS-CTX.RD-KEY 1 + C@ .',
            '." R2=" test-ctx @ TLS-CTX.RD-KEY 2 + C@ .',
            '." R3=" test-ctx @ TLS-CTX.RD-KEY 3 + C@ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("R0=127 ", text)
        self.assertIn("R1=190 ", text)
        self.assertIn("R2=91 ", text)
        self.assertIn("R3=227 ", text)

    def test_ks_handshake_server_inverts_record_directions(self):
        """Server WR uses S-HS while RD retains the peer C-HS epoch."""
        lines = self._TLS_KS_SETUP + [
            "TLS-ROLE-SERVER test-ctx @ TLS-CTX.ROLE !",
            "37 test-ctx @ TLS-CTX.WR-SEQ !",
            "41 test-ctx @ TLS-CTX.RD-SEQ !",
            'test-ctx @ TLS-KS-HANDSHAKE ." KS=" .',
            '." W0=" test-ctx @ TLS-CTX.WR-KEY C@ .',
            '." W3=" test-ctx @ TLS-CTX.WR-KEY 3 + C@ .',
            '." R0=" test-ctx @ TLS-CTX.RD-KEY C@ .',
            '." R3=" test-ctx @ TLS-CTX.RD-KEY 3 + C@ .',
            '." WI0=" test-ctx @ TLS-CTX.WR-IV C@ .',
            '." RI0=" test-ctx @ TLS-CTX.RD-IV C@ .',
            '." WS=" test-ctx @ TLS-CTX.WR-SEQ @ .',
            '." RS=" test-ctx @ TLS-CTX.RD-SEQ @ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("KS=0 ", text)
        self.assertIn("W0=127 ", text)
        self.assertIn("W3=227 ", text)
        self.assertIn("R0=245 ", text)
        self.assertIn("R3=81 ", text)
        self.assertIn("WI0=11 ", text)
        self.assertIn("RI0=77 ", text)
        self.assertIn("WS=0 ", text)
        self.assertIn("RS=0 ", text)

    def test_ks_handshake_unsealed_role_preserves_record_epoch(self):
        """An invalid role is rejected before any record-key mutation."""
        lines = self._TLS_KS_SETUP + [
            "TLS-ROLE-NONE test-ctx @ TLS-CTX.ROLE !",
            "test-ctx @ TLS-CTX.WR-KEY 32 90 FILL",
            "test-ctx @ TLS-CTX.RD-KEY 32 91 FILL",
            "17 test-ctx @ TLS-CTX.WR-SEQ !",
            "19 test-ctx @ TLS-CTX.RD-SEQ !",
            'test-ctx @ TLS-KS-HANDSHAKE ." KS=" .',
            '." WR=" test-ctx @ TLS-CTX.WR-KEY C@ .',
            '." RD=" test-ctx @ TLS-CTX.RD-KEY C@ .',
            '." WS=" test-ctx @ TLS-CTX.WR-SEQ @ .',
            '." RS=" test-ctx @ TLS-CTX.RD-SEQ @ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("KS=-4204 ", text)
        self.assertIn("WR=90 ", text)
        self.assertIn("RD=91 ", text)
        self.assertIn("WS=17 ", text)
        self.assertIn("RS=19 ", text)

    def test_ks_handshake_admitted_failure_is_not_retryable(self):
        """An admitted schedule failure is terminal after secret cleanup."""
        lines = self._TLS_KS_SETUP + [
            "test-ctx @ _TKSH-CTX !",
            "test-ctx @ TLS-CTX.WR-KEY 32 90 FILL",
            "test-ctx @ TLS-CTX.RD-KEY 32 91 FILL",
            '-777 _TKSH-FAIL ." FAIL=" .',
            ': zero32? 32 0 DO DUP I + C@ IF DROP 0 UNLOOP EXIT THEN '
            'LOOP DROP -1 ;',
            '." ERROR=" test-ctx @ TLS-CTX.ERROR @ .',
            '." WR=" test-ctx @ TLS-CTX.WR-KEY zero32? .',
            '." RD=" test-ctx @ TLS-CTX.RD-KEY zero32? .',
            "init-shared",
            'test-ctx @ TLS-KS-HANDSHAKE ." RETRY=" .',
            '." ERROR2=" test-ctx @ TLS-CTX.ERROR @ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("FAIL=-777 ", text)
        self.assertIn("ERROR=-777 ", text)
        self.assertIn("WR=-1 ", text)
        self.assertIn("RD=-1 ", text)
        self.assertIn("RETRY=-4204 ", text)
        self.assertIn("ERROR2=-777 ", text)

    def test_ks_handshake_wr_iv(self):
        """TLS-KS-HANDSHAKE derives correct client HS IV."""
        lines = self._TLS_KS_SETUP + [
            "test-ctx @ TLS-KS-HANDSHAKE DROP",
            '." I0=" test-ctx @ TLS-CTX.WR-IV C@ .',
            '." I1=" test-ctx @ TLS-CTX.WR-IV 1 + C@ .',
            '." I2=" test-ctx @ TLS-CTX.WR-IV 2 + C@ .',
            '." I3=" test-ctx @ TLS-CTX.WR-IV 3 + C@ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("I0=77 ", text)
        self.assertIn("I1=146 ", text)
        self.assertIn("I2=230 ", text)
        self.assertIn("I3=138 ", text)

    def test_ks_handshake_rd_iv(self):
        """TLS-KS-HANDSHAKE derives correct server HS IV."""
        lines = self._TLS_KS_SETUP + [
            "test-ctx @ TLS-KS-HANDSHAKE DROP",
            '." J0=" test-ctx @ TLS-CTX.RD-IV C@ .',
            '." J1=" test-ctx @ TLS-CTX.RD-IV 1 + C@ .',
            '." J2=" test-ctx @ TLS-CTX.RD-IV 2 + C@ .',
            '." J3=" test-ctx @ TLS-CTX.RD-IV 3 + C@ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("J0=11 ", text)
        self.assertIn("J1=205 ", text)
        self.assertIn("J2=119 ", text)
        self.assertIn("J3=228 ", text)

    def test_build_ch_record_header(self):
        """TLS-BUILD-CLIENT-HELLO produces correct TLS record header."""
        lines = [
            "VARIABLE test-ctx  0 TLS-CTX@ test-ctx !",
            "0 TLS-SNI-LEN !",
            "test-ctx @ TLS-BUILD-CLIENT-HELLO",
            'VARIABLE ch-len  ch-len !',
            'VARIABLE ch-addr  ch-addr !',
            '." LEN=" ch-len @ .',
            '." CT=" ch-addr @ C@ .',
            '." V0=" ch-addr @ 1 + C@ .',
            '." V1=" ch-addr @ 2 + C@ .',
            '." HT=" ch-addr @ 5 + C@ .',
        ]
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("LEN=173 ", text)
        self.assertIn("CT=22 ", text)     # ContentType=handshake
        self.assertIn("V0=3 ", text)      # 0x0301
        self.assertIn("V1=1 ", text)
        self.assertIn("HT=1 ", text)      # ClientHello

    def test_build_ch_cipher_suite(self):
        """The default ClientHello offers only standard TLS 1.3."""
        lines = [
            "VARIABLE test-ctx  0 TLS-CTX@ test-ctx !",
            "0 TLS-SNI-LEN !",
            "test-ctx @ TLS-BUILD-CLIENT-HELLO  2DROP",
            '." SL=" TLS-CH-BUF 77 + C@ .',       # suites_len low byte
            '." CS0=" TLS-CH-BUF 78 + C@ .',      # 0x13
            '." CS1=" TLS-CH-BUF 79 + C@ .',      # 0x01
            '." COMP=" TLS-CH-BUF 80 + C@ .',     # compression list len
        ]
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("SL=2 ", text)       # 2 bytes = 1 suite
        self.assertIn("CS0=19 ", text)     # 0x13
        self.assertIn("CS1=1 ", text)      # 0x01 (standard)
        self.assertIn("COMP=1 ", text)

    def test_build_ch_extensions(self):
        """Standard ClientHello contains the bounded public extensions."""
        lines = [
            "VARIABLE test-ctx  0 TLS-CTX@ test-ctx !",
            "0 TLS-SNI-LEN !",
            "test-ctx @ TLS-BUILD-CLIENT-HELLO  2DROP",
            '." EH=" TLS-CH-BUF 82 + C@ .',           # extensions_len high
            '." EL=" TLS-CH-BUF 83 + C@ .',           # extensions_len low
            '." SV=" TLS-CH-BUF 85 + C@ .',           # supported_versions type low
        ]
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("EH=0 ", text)
        self.assertIn("EL=89 ", text)
        self.assertIn("SV=43 ", text)    # 0x2B

    def test_build_ch_transcript_length(self):
        """After building CH, transcript contains the exact handshake bytes."""
        lines = [
            "VARIABLE test-ctx  0 TLS-CTX@ test-ctx !",
            "0 TLS-SNI-LEN !",
            "test-ctx @ TLS-BUILD-CLIENT-HELLO  2DROP",
            '." TL=" TLS-HS-TR-LEN @ .',
        ]
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("TL=168 ", text)

    def test_parse_sh_extracts_pubkey(self):
        """TLS-PARSE-SERVER-HELLO extracts X25519 peer pubkey."""
        # Build a crafted ServerHello: type=2, len=86, version=0x0303,
        # random=0*32, sid_len=0, cipher=0xFF01, comp=0,
        # extensions: supported_versions(0x0304) + key_share(x25519, pubkey)
        lines = [
            "VARIABLE test-ctx  0 TLS-CTX@ test-ctx !",
            "TLS-ROLE-CLIENT test-ctx @ TLS-CTX.ROLE !",
            "TLS-HELLO-HYBRID test-ctx @ TLS-CTX.HELLO-PROFILE !",
            "CREATE sh-buf 96 ALLOT",
            "sh-buf 96 0 FILL",
            # Handshake header
            "2 sh-buf C!",                                # type=ServerHello
            "0 sh-buf 1 + C!  0 sh-buf 2 + C!  86 sh-buf 3 + C!",  # len=86
            # Body
            "3 sh-buf 4 + C!  3 sh-buf 5 + C!",          # version 0x0303
            # random = 0 (already)
            "0 sh-buf 38 + C!",                            # sid_len=0
            "255 sh-buf 39 + C!  1 sh-buf 40 + C!",      # cipher=0xFF01
            "0 sh-buf 41 + C!",                            # comp=0
            "0 sh-buf 42 + C!  46 sh-buf 43 + C!",       # ext_len=46
            # supported_versions ext (6 bytes)
            "0 sh-buf 44 + C!  43 sh-buf 45 + C!",       # type=0x002B
            "0 sh-buf 46 + C!  2 sh-buf 47 + C!",        # len=2
            "3 sh-buf 48 + C!  4 sh-buf 49 + C!",        # 0x0304
            # key_share ext (40 bytes)
            "0 sh-buf 50 + C!  51 sh-buf 51 + C!",       # type=0x0033
            "0 sh-buf 52 + C!  36 sh-buf 53 + C!",       # len=36
            "0 sh-buf 54 + C!  29 sh-buf 55 + C!",       # group=x25519
            "0 sh-buf 56 + C!  32 sh-buf 57 + C!",       # key_len=32
            # pubkey = 66..97 (0x42..0x61)
            ": fill-pk 32 0 DO I 66 + sh-buf 58 + I + C! LOOP ; fill-pk",
            # Parse
            "VARIABLE sh-depth  DEPTH sh-depth !",
            "test-ctx @  sh-buf 90  TLS-PARSE-SERVER-HELLO",
            '." F=" .',
            'DEPTH sh-depth @ = ." SH-BALANCED=" .',
            '." PK0=" test-ctx @ TLS-CTX.PEER-PUBKEY C@ .',
            '." PK1=" test-ctx @ TLS-CTX.PEER-PUBKEY 1 + C@ .',
            '." PK31=" test-ctx @ TLS-CTX.PEER-PUBKEY 31 + C@ .',
            '." SUITE=" test-ctx @ TLS-CTX.SUITE @ .',
            '." HASH=" test-ctx @ TLS-CTX.HASH-ID @ .',
            "0 test-ctx @ TLS-CTX.SUITE !",
            "0 test-ctx @ TLS-CTX.HASH-ID !",
            "TLS-HELLO-STANDARD test-ctx @ TLS-CTX.HELLO-PROFILE !",
            "19 sh-buf 39 + C! 1 sh-buf 40 + C!",
            "test-ctx @ sh-buf 90 TLS-PARSE-SERVER-HELLO",
            '." STANDARD=" .',
            '." SSUITE=" test-ctx @ TLS-CTX.SUITE @ .',
            '." SHASH=" test-ctx @ TLS-CTX.HASH-ID @ .',
            "0 test-ctx @ TLS-CTX.SUITE !",
            "0 test-ctx @ TLS-CTX.HASH-ID !",
            "87 sh-buf 3 + C! 47 sh-buf 43 + C!",
            "test-ctx @ sh-buf 91 TLS-PARSE-SERVER-HELLO",
            '." LATE=" .',
            '." LSUITE=" test-ctx @ TLS-CTX.SUITE @ .',
            '." LHASH=" test-ctx @ TLS-CTX.HASH-ID @ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("F=0 ", text)        # success
        self.assertIn("SH-BALANCED=-1 ", text)
        self.assertIn("PK0=66 ", text)     # first pubkey byte
        self.assertIn("PK1=67 ", text)
        self.assertIn("PK31=97 ", text)    # last pubkey byte
        self.assertIn("SUITE=65281 ", text)
        self.assertIn("HASH=2 ", text)
        self.assertIn("STANDARD=0 ", text)
        self.assertIn("SSUITE=4865 ", text)
        self.assertIn("SHASH=1 ", text)
        self.assertIn("LATE=-1 ", text)
        self.assertIn("LSUITE=0 ", text)
        self.assertIn("LHASH=0 ", text)

    def test_parse_sh_bad_suite_rejected(self):
        """ServerHello with wrong cipher suite is rejected."""
        lines = [
            "VARIABLE test-ctx  0 TLS-CTX@ test-ctx !",
            "TLS-ROLE-CLIENT test-ctx @ TLS-CTX.ROLE !",
            "TLS-HELLO-HYBRID test-ctx @ TLS-CTX.HELLO-PROFILE !",
            "CREATE sh-buf 96 ALLOT",
            "sh-buf 96 0 FILL",
            "2 sh-buf C!",
            "0 sh-buf 1 + C!  0 sh-buf 2 + C!  86 sh-buf 3 + C!",
            "3 sh-buf 4 + C!  3 sh-buf 5 + C!",
            "0 sh-buf 38 + C!",
            # Wrong cipher suite: 0xFF02 instead of 0xFF01
            "255 sh-buf 39 + C!  2 sh-buf 40 + C!",
            "0 sh-buf 41 + C!",
            "0 sh-buf 42 + C!  46 sh-buf 43 + C!",
            "0 sh-buf 44 + C!  43 sh-buf 45 + C!",
            "0 sh-buf 46 + C!  2 sh-buf 47 + C!",
            "3 sh-buf 48 + C!  4 sh-buf 49 + C!",
            "0 sh-buf 50 + C!  51 sh-buf 51 + C!",
            "0 sh-buf 52 + C!  36 sh-buf 53 + C!",
            "0 sh-buf 54 + C!  29 sh-buf 55 + C!",
            "0 sh-buf 56 + C!  32 sh-buf 57 + C!",
            ": fill-pk 32 0 DO I 66 + sh-buf 58 + I + C! LOOP ; fill-pk",
            "test-ctx @  sh-buf 90  TLS-PARSE-SERVER-HELLO",
            '." F=" .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("F=-1 ", text)       # rejected

    def test_verify_finished_ok(self):
        """TLS-VERIFY-FINISHED accepts correct server Finished MAC."""
        # Use known key schedule state and pre-computed verify_data
        # S_VERIFY_DATA from Python: [189, 227, 95, 172, 210, 27, 22, 137,
        #   29, 38, 21, 194, 9, 172, 198, 38, 125, 204, 199, 254,
        #   58, 152, 166, 226, 87, 177, 185, 223, 238, 193, 79, 94]
        s_verify = [189, 227, 95, 172, 210, 27, 22, 137,
                    29, 38, 21, 194, 9, 172, 198, 38,
                    125, 204, 199, 254, 58, 152, 166, 226,
                    87, 177, 185, 223, 238, 193, 79, 94]
        lines = self._TLS_KS_SETUP + [
            "test-ctx @ TLS-KS-HANDSHAKE DROP",
            # Build fake verify_data with pre-computed bytes
            "CREATE vd-buf 32 ALLOT",
        ]
        for i, b in enumerate(s_verify):
            lines.append(f"{b} vd-buf {i} + C!")
        lines += [
            "test-ctx @ test-ctx @ TLS-CTX.S-HS-TRAFFIC  vd-buf"
            " TLS-VERIFY-FINISHED",
            '." VF=" .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("VF=0 ", text)       # valid

    def test_verify_finished_bad_mac(self):
        """TLS-VERIFY-FINISHED rejects wrong verify_data."""
        lines = self._TLS_KS_SETUP + [
            "test-ctx @ TLS-KS-HANDSHAKE DROP",
            # verify_data = all zeros (wrong)
            "CREATE vd-buf 32 ALLOT  vd-buf 32 0 FILL",
            "test-ctx @ test-ctx @ TLS-CTX.S-HS-TRAFFIC  vd-buf"
            " TLS-VERIFY-FINISHED",
            '." VF=" .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("VF=-1 ", text)      # rejected

    def test_supplied_digest_cores_match_legacy_and_snapshot_aliases(self):
        """Digest-fed derivation/Finished cores are exact and alias-stable."""
        lines = self._TLS_KS_SETUP + [
            "CREATE supplied-hash 32 ALLOT",
            "CREATE supplied-secret 32 ALLOT",
            "CREATE supplied-a 32 ALLOT",
            "CREATE supplied-b 32 ALLOT",
            "test-ctx @ TLS-HS-TRANSCRIPT TLS-HS-TR-LEN @ supplied-hash "
            "TLS-HASH DROP",
            "32 0 DO I 1+ supplied-secret I + C! LOOP",
            "test-ctx @ supplied-secret TLS-L-C-HS-TR /TLS-L-C-HS-TR "
            "supplied-hash supplied-a (TLS-DERIVE-SECRET-HASH) DROP",
            "test-ctx @ supplied-secret TLS-L-C-HS-TR /TLS-L-C-HS-TR "
            "supplied-b TLS-DERIVE-SECRET DROP",
            '." DERIVE=" supplied-a supplied-b 32 _XC-BYTES= .',
            "supplied-hash TLS-FINISHED-KEY 32 MOVE",
            "test-ctx @ supplied-secret TLS-FINISHED-KEY supplied-a "
            "(TLS-FINISHED-MAC-HASH) DROP",
            "test-ctx @ supplied-secret supplied-hash supplied-b "
            "(TLS-FINISHED-MAC-HASH) DROP",
            '." FIN=" supplied-a supplied-b 32 _XC-BYTES= .',
            "supplied-b TLS-VERIFY-DATA 32 MOVE",
            "test-ctx @ supplied-secret supplied-hash TLS-VERIFY-DATA "
            "(TLS-VERIFY-FINISHED-HASH)",
            '." VERIFY-ALIAS=" .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("DERIVE=-1 ", text)
        self.assertIn("FIN=-1 ", text)
        self.assertIn("VERIFY-ALIAS=0 ", text)

    def test_build_finished_format(self):
        """TLS-BUILD-FINISHED produces encrypted record with correct header."""
        lines = self._TLS_KS_SETUP + [
            "test-ctx @ TLS-KS-HANDSHAKE DROP",
            "CREATE fin-rec 128 ALLOT",
            "test-ctx @  fin-rec  TLS-BUILD-FINISHED",
            "VARIABLE fin-rl  fin-rl !",
            '." RL=" fin-rl @ .',
            # Record header: type=23 (app_data), version=0x0303
            '." RT=" fin-rec C@ .',
            '." RV0=" fin-rec 1 + C@ .',
            '." RV1=" fin-rec 2 + C@ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("RT=23 ", text)      # app_data outer type
        self.assertIn("RV0=3 ", text)      # 0x0303
        self.assertIn("RV1=3 ", text)

    def test_build_finished_uses_server_handshake_write_secret(self):
        """Server Finished encrypts and authenticates under S-HS."""
        lines = self._TLS_KS_SETUP + [
            "TLS-ROLE-SERVER test-ctx @ TLS-CTX.ROLE !",
            "test-ctx @ TLS-KS-HANDSHAKE DROP",
            "CREATE srv-fin-rec 128 ALLOT",
            "CREATE srv-fin-plain 64 ALLOT",
            "VARIABLE peer-ctx 1 TLS-CTX@ peer-ctx !",
            "peer-ctx @ /TLS-CTX 0 FILL",
            "TLS-ROLE-CLIENT peer-ctx @ TLS-CTX.ROLE !",
            "TLS-HASH-SHA3-256 peer-ctx @ TLS-CTX.HASH-ID !",
            "TLS-SUITE-X25519-SHA3 peer-ctx @ TLS-CTX.SUITE !",
            "test-ctx @ srv-fin-rec TLS-BUILD-FINISHED",
            "VARIABLE srv-fin-len srv-fin-len !",
            "test-ctx @ TLS-CTX.WR-KEY peer-ctx @ TLS-CTX.RD-KEY 32 MOVE",
            "test-ctx @ TLS-CTX.WR-IV peer-ctx @ TLS-CTX.RD-IV 16 MOVE",
            "peer-ctx @ srv-fin-rec srv-fin-len @ srv-fin-plain "
            "TLS-DECRYPT-RECORD",
            '." PL=" . ." CT=" .',
            '." HT=" srv-fin-plain C@ .',
            "test-ctx @ test-ctx @ TLS-CTX.S-HS-TRAFFIC "
            "srv-fin-plain 4 + TLS-VERIFY-FINISHED",
            '." GOOD=" .',
            "test-ctx @ test-ctx @ TLS-CTX.C-HS-TRAFFIC "
            "srv-fin-plain 4 + TLS-VERIFY-FINISHED",
            '." WRONG=" .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("CT=22 ", text)
        self.assertIn("PL=36 ", text)
        self.assertIn("HT=20 ", text)
        self.assertIn("GOOD=0 ", text)
        self.assertIn("WRONG=-1 ", text)

    def test_ctx_size_updated(self):
        """TLS context includes transport and socket incarnation authority."""
        text = self._run_kdos(['." SZ=" /TLS-CTX .'])
        self.assertIn("SZ=1000 ", text)

    def test_label_strings_correct(self):
        """TLS label constants contain correct ASCII bytes."""
        lines = [
            # "derived" = 100 101 114 105 118 101 100
            '." D0=" TLS-L-DERIVED C@ .',
            '." D6=" TLS-L-DERIVED 6 + C@ .',
            # "key" = 107 101 121
            '." K0=" TLS-L-KEY C@ .',
            '." K2=" TLS-L-KEY 2 + C@ .',
            # "finished" = 102 105 110 105 115 104 101 100
            '." F0=" TLS-L-FINISHED C@ .',
            '." F7=" TLS-L-FINISHED 7 + C@ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("D0=100 ", text)     # 'd'
        self.assertIn("D6=100 ", text)     # 'd'
        self.assertIn("K0=107 ", text)     # 'k'
        self.assertIn("K2=121 ", text)     # 'y'
        self.assertIn("F0=102 ", text)     # 'f'
        self.assertIn("F7=100 ", text)     # 'd'

    def test_handshake_state_after_ch(self):
        """After building ClientHello, handshake state is CLIENT-HELLO-SENT."""
        lines = [
            "VARIABLE test-ctx  0 TLS-CTX@ test-ctx !",
            "test-ctx @ TLS-BUILD-CLIENT-HELLO  2DROP",
            '." ST=" test-ctx @ TLS-CTX.STATE @ .',
            '." HS=" test-ctx @ TLS-CTX.HS-STATE @ .',
        ]
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("ST=1 ", text)       # TLSS-HANDSHAKE
        self.assertIn("HS=1 ", text)       # TLSH-CLIENT-HELLO-SENT

    def test_finished_cannot_bypass_certificate_states(self):
        """A Finished message is rejected until certificate proof succeeds."""
        lines = [
            "VARIABLE test-ctx 0 TLS-CTX@ test-ctx !",
            "test-ctx @ /TLS-CTX 0 FILL",
            "TLSS-HANDSHAKE test-ctx @ TLS-CTX.STATE !",
            "TLSH-SERVER-HELLO-RCVD test-ctx @ TLS-CTX.HS-STATE !",
            "CREATE early-fin 36 ALLOT early-fin 36 0 FILL",
            "20 early-fin C! 32 early-fin 3 + C!",
            "test-ctx @ early-fin 36 TLS-PROCESS-HS-MSG",
            '." FLAG=" .',
            '." AUTH=" test-ctx @ TLS-CTX.PEER-AUTH @ .',
            '." HS=" test-ctx @ TLS-CTX.HS-STATE @ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("FLAG=-1 ", text)
        self.assertIn("AUTH=0 ", text)
        self.assertIn("HS=2 ", text)

    def test_application_keys_require_peer_authentication(self):
        """The application key schedule cannot establish an unauthenticated ctx."""
        lines = [
            "VARIABLE test-ctx 0 TLS-CTX@ test-ctx !",
            "test-ctx @ /TLS-CTX 0 FILL",
            "TLSS-HANDSHAKE test-ctx @ TLS-CTX.STATE !",
            "TLSH-SERVER-FINISHED test-ctx @ TLS-CTX.HS-STATE !",
            '." KS=" test-ctx @ TLS-KS-APPLICATION .',
            '." STATE=" test-ctx @ TLS-CTX.STATE @ .',
            "CREATE fin-rec 128 ALLOT",
            "test-ctx @ fin-rec TLS-HANDSHAKE-COMPLETE",
            '." RECLEN=" .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("KS=-4204 ", text)
        self.assertIn("STATE=1 ", text)
        self.assertIn("RECLEN=0 ", text)

    def test_client_hello_advertises_separate_supported_signatures(self):
        """Handshake and certificate signature offers are exact and distinct:
        ECDSA P-256 and P-384 and RSA-PSS with SHA-256, -384 and -512 for
        CertificateVerify; ECDSA and RSA PKCS#1 v1.5 for certificates."""
        lines = [
            "VARIABLE test-ctx 0 TLS-CTX@ test-ctx !",
            "0 TLS-SNI-LEN !",
            "test-ctx @ TLS-BUILD-CLIENT-HELLO 2DROP",
            '." TYPE=" TLS-CH-BUF 134 + C@ .',
            '." EXTLEN=" TLS-CH-BUF 136 + C@ .',
            '." LISTLEN=" TLS-CH-BUF 138 + C@ .',
            '." ALGS=" ' + ' '.join(
                f'TLS-CH-BUF {139 + i} + C@ .' for i in range(10)),
            '." CTYPE=" TLS-CH-BUF 150 + C@ .',
            '." CLEN=" TLS-CH-BUF 152 + C@ .',
            '." CLIST=" TLS-CH-BUF 154 + C@ .',
            '." CALGS=" ' + ' '.join(
                f'TLS-CH-BUF {155 + i} + C@ .' for i in range(10)),
        ]
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("TYPE=13 ", text)
        self.assertIn("EXTLEN=12 ", text)
        self.assertIn("LISTLEN=10 ", text)
        self.assertIn("ALGS=4 3 5 3 8 4 8 5 8 6 ", text)
        self.assertIn("CTYPE=50 ", text)
        self.assertIn("CLEN=12 ", text)
        self.assertIn("CLIST=10 ", text)
        self.assertIn("CALGS=4 3 5 3 4 1 5 1 6 1 ", text)

    def test_http11_alpn_profile_roundtrip(self):
        """HTTP callers advertise and require an exact http/1.1 selection."""
        lines = [
            "VARIABLE test-ctx 0 TLS-CTX@ test-ctx !",
            "0 TLS-SNI-LEN !",
            "TLS-ALPN-HTTP11 test-ctx @ TLS-CTX.ALPN-PROFILE !",
            "test-ctx @ TLS-BUILD-CLIENT-HELLO",
            '." CHLEN=" . DROP',
            '." EXTLEN=" TLS-CH-BUF 83 + C@ .',
            '." ATYPE=" TLS-CH-BUF 174 + C@ .',
            '." ANAME=" TLS-CH-BUF 180 + 8 TYPE CR',
            "CREATE ee 21 ALLOT ee 21 0 FILL",
            "8 ee C! 17 ee 3 + C! 15 ee 5 + C!",
            "16 ee 7 + C! 11 ee 9 + C! 9 ee 11 + C! 8 ee 12 + C!",
            "TLS-ALPN-HTTP11-NAME ee 13 + 8 CMOVE",
            "test-ctx @ ee 21 TLS-PARSE-ENCRYPTED-EXT",
            '." EE=" .',
            '." NEG=" test-ctx @ TLS-CTX.ALPN-NEGOTIATED @ .',
            "CREATE no-ee 6 ALLOT no-ee 6 0 FILL",
            "8 no-ee C! 2 no-ee 3 + C!",
            "test-ctx @ no-ee 6 TLS-PARSE-ENCRYPTED-EXT",
            '." MISSING=" .',
        ]
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("CHLEN=188 ", text)
        self.assertIn("EXTLEN=104 ", text)
        self.assertIn("ATYPE=16 ", text)
        self.assertIn("ANAME=http/1.1", text)
        self.assertIn("EE=0 ", text)
        self.assertIn("NEG=1 ", text)
        self.assertIn("MISSING=-1 ", text)

    def test_public_sni_alpn_client_hello_shape(self):
        """Codex-shaped public ClientHello has exact nested lengths."""
        lines = [
            "VARIABLE test-ctx 0 TLS-CTX@ test-ctx !",
            "test-ctx @ /TLS-CTX 0 FILL",
            "TLS-ALPN-HTTP11 test-ctx @ TLS-CTX.ALPN-PROFILE !",
            "15 TLS-SNI-LEN !",
            'S" auth.openai.com" TLS-SNI-HOST SWAP CMOVE',
            "test-ctx @ TLS-BUILD-CLIENT-HELLO",
            '." CHLEN=" . DROP',
            '." RECORD=" TLS-CH-BUF 3 + NW16@ .',
            '." BODY=" TLS-CH-BUF 6 + _BE24@ .',
            '." EXTS=" TLS-CH-BUF 82 + NW16@ .',
            '." SNI=" TLS-CH-BUF 93 + 15 TYPE CR',
            '." ALPN=" TLS-CH-BUF 204 + 8 TYPE CR',
        ]
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("CHLEN=212 ", text)
        self.assertIn("RECORD=207 ", text)
        self.assertIn("BODY=203 ", text)
        self.assertIn("EXTS=128 ", text)
        self.assertIn("SNI=auth.openai.com", text)
        self.assertIn("ALPN=http/1.1", text)

    def test_client_hello_admits_dns_max_sni_without_overflow(self):
        """The DNS maximum fits every profile and oversize fails pre-write."""
        lines = [
            "VARIABLE test-ctx 0 TLS-CTX@ test-ctx !",
            "test-ctx @ /TLS-CTX 0 FILL",
            "TLS-ALPN-HTTP11 test-ctx @ TLS-CTX.ALPN-PROFILE !",
            ": max-sni",
            "  TLS-SNI-HOST TLS-SNI-HOST-STORAGE 0 FILL",
            "  TLS-SNI-HOST TLS-SNI-HOST-CAPACITY 97 FILL",
            "  46 TLS-SNI-HOST 63 + C!",
            "  46 TLS-SNI-HOST 127 + C!",
            "  46 TLS-SNI-HOST 191 + C!",
            ";",
            "max-sni",
            "TLS-SNI-HOST-CAPACITY TLS-SNI-LEN !",
            '." DNS=" TLS-SNI-HOST TLS-SNI-LEN @ FALSE DNS-NAME-VALID? .',
            '." WORST=" 927 TLS-SNI-HOST-CAPACITY 9 + + 15 + 77 + 9 + .',
            '." CAP=" TLS-CH-BUF-CAPACITY .',
            "TLS-HELLO-STANDARD test-ctx @ TLS-CTX.HELLO-PROFILE !",
            "TLS-CH-BUF TLS-CH-BUF-CAPACITY 165 FILL",
            "test-ctx @ TLS-BUILD-CLIENT-HELLO",
            "VARIABLE max-ch-len max-ch-len !",
            "VARIABLE max-ch-addr max-ch-addr !",
            '." LEN=" max-ch-len @ .',
            '." RECORD=" TLS-CH-BUF 3 + NW16@ .',
            '." FIRST=" TLS-CH-BUF 93 + C@ .',
            '." LAST=" TLS-CH-BUF 345 + C@ .',
            '." NEXT=" TLS-CH-BUF 450 + C@ .',
            '." END=" TLS-CH-BUF TLS-CH-BUF-CAPACITY + 1- C@ .',
            "TLS-CH-BUF TLS-CH-BUF-CAPACITY 90 FILL",
            "13 TLS-HS-TR-LEN !",
            "TLS-SNI-HOST-CAPACITY 1+ TLS-SNI-LEN !",
            "test-ctx @ TLS-BUILD-CLIENT-HELLO",
            '." BADLEN=" .',
            '." BADADDR=" .',
            '." BADFIRST=" TLS-CH-BUF C@ .',
            '." BADEND=" TLS-CH-BUF TLS-CH-BUF-CAPACITY + 1- C@ .',
            '." BADTR=" TLS-HS-TR-LEN @ .',
            '." BADSTATE=" test-ctx @ TLS-CTX.STATE @ .',
        ]
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("DNS=-1 ", text)
        self.assertIn("WORST=1290 ", text)
        self.assertIn("CAP=1537 ", text)
        self.assertIn("LEN=450 ", text)
        self.assertIn("RECORD=445 ", text)
        self.assertIn("FIRST=97 ", text)
        self.assertIn("LAST=97 ", text)
        self.assertIn("NEXT=165 ", text)
        self.assertIn("END=165 ", text)
        self.assertIn("BADLEN=0 ", text)
        self.assertIn("BADADDR=0 ", text)
        self.assertIn("BADFIRST=90 ", text)
        self.assertIn("BADEND=90 ", text)
        self.assertIn("BADTR=13 ", text)
        self.assertIn("BADSTATE=1 ", text)

    def test_build_ch_hybrid_key_share_group(self):
        """Explicit hybrid ClientHello uses the private ML-KEM group."""
        # key_share ext starts at offset 93 (86+7 for supported_versions)
        # ext_type(2) + ext_len(2) + entries_len(2) = 6 bytes header
        # first entry group at offset 93+6 = 99
        lines = [
            "VARIABLE test-ctx  0 TLS-CTX@ test-ctx !",
            "0 TLS-SNI-LEN !",
            "TLS-HELLO-HYBRID test-ctx @ TLS-CTX.HELLO-PROFILE !",
            "test-ctx @ TLS-BUILD-CLIENT-HELLO  2DROP",
            '." G0=" TLS-CH-BUF 99 + C@ .',       # group high = 0xFE
            '." G1=" TLS-CH-BUF 100 + C@ .',      # group low = 0x00
            '." KH=" TLS-CH-BUF 101 + C@ .',      # key_len high = 3
            '." KL=" TLS-CH-BUF 102 + C@ .',      # key_len low  = 64 (0x40)
        ]
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("G0=254 ", text)
        self.assertIn("G1=0 ", text)
        self.assertIn("KH=3 ", text)       # 832 = 0x0340
        self.assertIn("KL=64 ", text)

    def test_build_ch_supported_groups_has_hybrid(self):
        """Explicit hybrid supported_groups lists private ML-KEM first."""
        # key_share is followed by 16-byte handshake and certificate
        # signature extensions.  The groups list follows both exact offers.
        lines = [
            "VARIABLE test-ctx  0 TLS-CTX@ test-ctx !",
            "0 TLS-SNI-LEN !",
            "TLS-HELLO-HYBRID test-ctx @ TLS-CTX.HELLO-PROFILE !",
            "test-ctx @ TLS-BUILD-CLIENT-HELLO  2DROP",
            '." EH=" TLS-CH-BUF 84 + C@ .',       # 927 = 0x039F
            '." EL=" TLS-CH-BUF 85 + C@ .',
            '." SG0=" TLS-CH-BUF 1009 + C@ .',    # hybrid high = 254
            '." SG1=" TLS-CH-BUF 1010 + C@ .',    # hybrid low = 0
            '." SG2=" TLS-CH-BUF 1011 + C@ .',    # x25519 high = 0
            '." SG3=" TLS-CH-BUF 1012 + C@ .',    # x25519 low  = 29
        ]
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("EH=3 ", text)
        self.assertIn("EL=159 ", text)
        self.assertIn("SG0=254 ", text)
        self.assertIn("SG1=0 ", text)
        self.assertIn("SG2=0 ", text)
        self.assertIn("SG3=29 ", text)

    def test_parse_sh_hybrid_group(self):
        """TLS-PARSE-SERVER-HELLO accepts the private hybrid key share."""
        # Build crafted SH with hybrid group:
        # key_data = x25519_pub(32) || kyber_ct(768) = 800 bytes
        lines = [
            "VARIABLE test-ctx  0 TLS-CTX@ test-ctx !",
            "TLS-ROLE-CLIENT test-ctx @ TLS-CTX.ROLE !",
            "TLS-HELLO-HYBRID test-ctx @ TLS-CTX.HELLO-PROFILE !",
            "CREATE sh-buf 900 ALLOT",
            "sh-buf 900 0 FILL",
            # Handshake header
            "2 sh-buf C!",
            "0 sh-buf 1 + C!  0 sh-buf 2 + C!",
            # hs_len = 4 + 2 + 32 + 1 + 2 + 1 + 2 + ext_len
            # body = version(2)+random(32)+sid_len(1)+cipher(2)+comp(1)+ext_len_field(2)+ext
            # ext = versions(6) + key_share(4+800=804) = 810
            # body = 2+32+0+2+1+2+810 = 849 ... let me compute properly
            # sid_len=0, cipher=0xFF01, comp=0
            # ext: supported_versions(6B) + key_share(808B) = 814
            # body_len = 2+32+0+1+2+1+2+814 = 854
            # Actually: version(2) + random(32) + sid_len(1=0) + cipher(2) + comp(1=0) +
            #   ext_len_field(2) + extensions
            # = 2+32+1+2+1+2+ext = 40+ext
            # ext = versions_ext(6B) + key_share_ext
            # versions_ext = type(2)+len(2)+data(2) = 6
            # key_share = type(2)+ext_len(2)+group(2)+key_len(2)+data(800) = 808
            # ext = 6+808 = 814
            # body_len = 40+814 = 854

            # hs_len(3 bytes) = 854
            "3 sh-buf 2 + C!  86 sh-buf 3 + C!",  # 854 = 0x0356 → 3, 86
            # version 0x0303
            "3 sh-buf 4 + C!  3 sh-buf 5 + C!",
            # random = 0 (already)
            # sid_len = 0
            "0 sh-buf 38 + C!",
            # cipher = 0xFF01
            "255 sh-buf 39 + C!  1 sh-buf 40 + C!",
            # comp = 0
            "0 sh-buf 41 + C!",
            # ext_len = 814 = 0x032E
            "3 sh-buf 42 + C!  46 sh-buf 43 + C!",
            # supported_versions (6B)
            "0 sh-buf 44 + C!  43 sh-buf 45 + C!",   # type=0x002B
            "0 sh-buf 46 + C!  2 sh-buf 47 + C!",    # len=2
            "3 sh-buf 48 + C!  4 sh-buf 49 + C!",    # 0x0304
            # key_share (808B)
            "0 sh-buf 50 + C!  51 sh-buf 51 + C!",   # type=0x0033
            "3 sh-buf 52 + C!  36 sh-buf 53 + C!",   # ext_len=804 (0x0324)
            "254 sh-buf 54 + C!  0 sh-buf 55 + C!", # group=0xFE00
            "3 sh-buf 56 + C!  32 sh-buf 57 + C!",   # key_len=800 (0x0320)
            # key_data[0..31] = X25519 pubkey (66..97)
            ": fill-pk 32 0 DO I 66 + sh-buf 58 + I + C! LOOP ; fill-pk",
            # key_data[32..799] = Kyber CT (fill with 0xAA)
            ": fill-ct 768 0 DO 170 sh-buf 90 + I + C! LOOP ; fill-ct",
            # Parse — total msg bytes = 4 + 854 = 858
            "test-ctx @  sh-buf 858  TLS-PARSE-SERVER-HELLO",
            '." F=" .',
            '." GRP=" TLS-HS-GROUP @ .',
            '." PK0=" test-ctx @ TLS-CTX.PEER-PUBKEY C@ .',
            '." CT0=" TLS-HS-KYBER-CT C@ .',
            '." CT767=" TLS-HS-KYBER-CT 767 + C@ .',
        ]
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("F=0 ", text)          # success
        self.assertIn("GRP=65024 ", text)    # TLS-GROUP-HYBRID-PQ
        self.assertIn("PK0=66 ", text)       # first X25519 pubkey byte
        self.assertIn("CT0=170 ", text)      # first CT byte (0xAA)
        self.assertIn("CT767=170 ", text)    # last CT byte

    def test_parse_sh_unknown_group_rejected(self):
        """ServerHello with unrecognized key_share group is rejected."""
        lines = [
            "VARIABLE test-ctx  0 TLS-CTX@ test-ctx !",
            "TLS-ROLE-CLIENT test-ctx @ TLS-CTX.ROLE !",
            "TLS-HELLO-HYBRID test-ctx @ TLS-CTX.HELLO-PROFILE !",
            "CREATE sh-buf 96 ALLOT",
            "sh-buf 96 0 FILL",
            "2 sh-buf C!",
            "0 sh-buf 1 + C!  0 sh-buf 2 + C!  86 sh-buf 3 + C!",
            "3 sh-buf 4 + C!  3 sh-buf 5 + C!",
            "0 sh-buf 38 + C!",
            "255 sh-buf 39 + C!  1 sh-buf 40 + C!",
            "0 sh-buf 41 + C!",
            "0 sh-buf 42 + C!  46 sh-buf 43 + C!",
            "0 sh-buf 44 + C!  43 sh-buf 45 + C!",
            "0 sh-buf 46 + C!  2 sh-buf 47 + C!",
            "3 sh-buf 48 + C!  4 sh-buf 49 + C!",
            "0 sh-buf 50 + C!  51 sh-buf 51 + C!",
            "0 sh-buf 52 + C!  36 sh-buf 53 + C!",
            # Unknown group 0x00FF
            "0 sh-buf 54 + C!  255 sh-buf 55 + C!",
            "0 sh-buf 56 + C!  32 sh-buf 57 + C!",
            ": fill-pk 32 0 DO I 66 + sh-buf 58 + I + C! LOOP ; fill-pk",
            "test-ctx @  sh-buf 90  TLS-PARSE-SERVER-HELLO",
            '." F=" .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("F=-1 ", text)

    def test_parse_sh_rejects_hrr_and_truncation(self):
        """HelloRetryRequest and undersized ServerHello fail explicitly."""
        lines = [
            "VARIABLE test-ctx 0 TLS-CTX@ test-ctx !",
            "TLS-ROLE-CLIENT test-ctx @ TLS-CTX.ROLE !",
            "CREATE sh-buf 44 ALLOT sh-buf 44 0 FILL",
            "2 sh-buf C! 40 sh-buf 3 + C!",
            "3 sh-buf 4 + C! 3 sh-buf 5 + C!",
            "TLS-HRR-RANDOM sh-buf 6 + 32 CMOVE",
            "test-ctx @ sh-buf 44 TLS-PARSE-SERVER-HELLO",
            '." HRR=" .',
            "test-ctx @ sh-buf 43 TLS-PARSE-SERVER-HELLO",
            '." SHORT=" .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("HRR=-1 ", text)
        self.assertIn("SHORT=-1 ", text)

    def test_transcript_overflow_is_sticky_and_explicit(self):
        """An oversized transcript cannot silently hash a truncated prefix."""
        lines = [
            "CREATE tr-byte 1 ALLOT",
            "TLS-TR-RESET",
            "tr-byte TLS-HS-TR-MAX 1+ TLS-TR-APPEND",
            '." ERROR=" TLS-HS-TR-ERROR @ .',
            '." LEN=" TLS-HS-TR-LEN @ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("ERROR=-1 ", text)
        self.assertIn("LEN=0 ", text)

    def test_encrypted_handshake_fragments_reassemble(self):
        """A handshake message may cross arbitrary protected-record boundaries."""
        lines = [
            "VARIABLE test-ctx 0 TLS-CTX@ test-ctx !",
            "test-ctx @ /TLS-CTX 0 FILL",
            "TLSS-HANDSHAKE test-ctx @ TLS-CTX.STATE !",
            "TLSH-SERVER-HELLO-RCVD test-ctx @ TLS-CTX.HS-STATE !",
            "TLS-ALPN-NONE test-ctx @ TLS-CTX.ALPN-PROFILE !",
            "TLS-HS-RBUF-RESET TLS-TR-RESET",
            "CREATE ee-empty 6 ALLOT ee-empty 6 0 FILL",
            "8 ee-empty C! 2 ee-empty 3 + C!",
            "test-ctx @ ee-empty 3 TLS-PROCESS-HS-MSGS",
            '." FIRST=" .',
            '." HELD=" TLS-HS-RBUF-LEN @ .',
            '." STATE1=" test-ctx @ TLS-CTX.HS-STATE @ .',
            "test-ctx @ ee-empty 3 + 3 TLS-PROCESS-HS-MSGS",
            '." SECOND=" .',
            '." LEFT=" TLS-HS-RBUF-LEN @ .',
            '." STATE2=" test-ctx @ TLS-CTX.HS-STATE @ .',
            "TLS-HS-RBUF-RESET",
            "CREATE huge-hs 4 ALLOT 8 huge-hs C!",
            "255 huge-hs 1+ C! 255 huge-hs 2 + C! 255 huge-hs 3 + C!",
            "test-ctx @ huge-hs 4 TLS-PROCESS-HS-MSGS",
            '." HUGE=" .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("FIRST=0 ", text)
        self.assertIn("HELD=3 ", text)
        self.assertIn("STATE1=2 ", text)
        self.assertIn("SECOND=0 ", text)
        self.assertIn("LEFT=0 ", text)
        self.assertIn("STATE2=3 ", text)
        self.assertIn("HUGE=-1 ", text)

    def test_hybrid_group_constants(self):
        """Hybrid PQ group constants have correct values."""
        text = self._run_kdos([
            '." X=" TLS-GROUP-X25519 .',
            '." H=" TLS-GROUP-HYBRID-PQ .',
        ])
        self.assertIn("X=29 ", text)       # 0x001D
        self.assertIn("H=65024 ", text)    # 0xFE00 private use

    def test_hs_group_default_after_ch(self):
        """TLS-HS-GROUP defaults to x25519 after ClientHello."""
        lines = [
            "VARIABLE test-ctx  0 TLS-CTX@ test-ctx !",
            "0 TLS-SNI-LEN !",
            "test-ctx @ TLS-BUILD-CLIENT-HELLO  2DROP",
            '." G=" TLS-HS-GROUP @ .',
        ]
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("G=29 ", text)       # TLS-GROUP-X25519 default


# ---------------------------------------------------------------------------
#  TLS 1.3 exporter tests — RFC 8446 §7.5
# ---------------------------------------------------------------------------

class TestKDOSTLSExporter(_KDOSNetworkTestBase):
    """Exporter derivation is authenticated, context-bound, and atomic."""

    @staticmethod
    def _forth_bytes(name, value):
        data = bytes.fromhex(value) if isinstance(value, str) else bytes(value)
        lines = [f"CREATE {name} {max(len(data), 1)} ALLOT"]
        lines.extend(f"{byte} {name} {offset} + C!"
                     for offset, byte in enumerate(data))
        return lines

    @staticmethod
    def _established_context():
        return [
            "VARIABLE exp-ctx 0 TLS-CTX@ exp-ctx !",
            "exp-ctx @ TLS-CTX-CLAIM 0= IF -1 THROW THEN",
            "TLS-ROLE-CLIENT exp-ctx @ TLS-CTX.ROLE !",
            "TLS-HASH-SHA256 exp-ctx @ TLS-CTX.HASH-ID !",
            "TLS-SUITE-AES128-SHA256 exp-ctx @ TLS-CTX.SUITE !",
            "1 exp-ctx @ TLS-CTX.PEER-AUTH !",
            "TLSH-CONNECTED exp-ctx @ TLS-CTX.HS-STATE !",
            "TLSS-ESTABLISHED exp-ctx @ TLS-CTX.STATE !",
        ]

    def test_rfc8448_schedule_and_rfc9266_exporter_anchor(self):
        """RFC 8448 schedule values reproduce the RFC 9266 exporter tuple."""
        master = (
            "18df06843d13a08bf2a449844c5f8a478001bc4d4c627984d5a41da8d0402919"
        )
        transcript_hash = (
            "9608102a0f1ccc6db6250b7b7e417b1a000eaada3daae4777a7686c9ff83df13"
        )
        exporter_master = (
            "fe22f881176eda18eb8f44529e6792c50c9a3f89452f68d8ae311b4309d3cf50"
        )
        channel_binding = (
            "e3b0946bf2f4668144f22872e0afd51dc9608638c6f9b2584b98c6cd3a4affad"
        )
        lines = self._established_context()
        lines += self._forth_bytes("rfc-master", master)
        lines += self._forth_bytes("rfc-transcript", transcript_hash)
        lines += self._forth_bytes("rfc-exp-master", exporter_master)
        lines += self._forth_bytes("rfc-exported", channel_binding)
        lines += self._forth_bytes(
            "rfc-label", b"EXPORTER-Channel-Binding"
        )
        lines += [
            "CREATE derived-exp 32 ALLOT derived-exp 32 165 FILL",
            "exp-ctx @ rfc-master TLS-L-EXP-MASTER /TLS-L-EXP-MASTER",
            "rfc-transcript 32 32 derived-exp TLS-EXPAND-LABEL",
            '."  DERIVE=" .',
            '."  EMS=" derived-exp rfc-exp-master 32 VERIFY .',
            "rfc-exp-master exp-ctx @ TLS-CTX.EXPORTER-MS 32 MOVE",
            "CREATE exported 32 ALLOT exported 32 165 FILL",
            "exp-ctx @ rfc-label 24 0 0 exported 32 TLS-EXPORT",
            '."  IOR=" .',
            '."  OUT=" exported rfc-exported 32 VERIFY .',
            ': exp-zero? 32 0 DO DUP I + C@ IF DROP 0 UNLOOP EXIT THEN LOOP '
            'DROP -1 ;',
            '."  INNER-WIPED=" TLS-EXPORT-INNER exp-zero? .',
            '."  HASH-WIPED=" TLS-EXPORT-CONTEXT-HASH exp-zero? .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("DERIVE=0 ", text)
        self.assertIn("EMS=0 ", text)
        self.assertIn("IOR=0 ", text)
        self.assertIn("OUT=0 ", text)
        self.assertIn("INNER-WIPED=-1 ", text)
        self.assertIn("HASH-WIPED=-1 ", text)

    def test_exporter_state_parameters_aliases_and_context_binding(self):
        """Misuse preserves output and raw context changes derived bytes."""
        exporter_master = (
            "fe22f881176eda18eb8f44529e6792c50c9a3f89452f68d8ae311b4309d3cf50"
        )
        lines = self._established_context()
        lines += self._forth_bytes("exp-master", exporter_master)
        lines += self._forth_bytes("test-label", b"test")
        lines += [
            "exp-master exp-ctx @ TLS-CTX.EXPORTER-MS 32 MOVE",
            "CREATE sentinel 64 ALLOT sentinel 64 90 FILL",
            "TLSS-HANDSHAKE exp-ctx @ TLS-CTX.STATE !",
            "exp-ctx @ test-label 4 0 0 sentinel 32 TLS-EXPORT",
            '."  EARLY=" .',
            '."  EARLY-OUT=" sentinel C@ .',
            "TLSS-ESTABLISHED exp-ctx @ TLS-CTX.STATE !",
            "exp-ctx @ test-label 4 0 0 0 0 TLS-EXPORT",
            '."  ZERO=" .',
            "CREATE bad-label 1 ALLOT 0 bad-label C!",
            "exp-ctx @ bad-label 1 0 0 sentinel 32 TLS-EXPORT",
            '."  BAD-LABEL=" .',
            "CREATE alias-label 64 ALLOT alias-label 64 97 FILL",
            "exp-ctx @ alias-label 4 0 0 alias-label 32 TLS-EXPORT",
            '."  ALIAS=" .',
            '."  ALIAS-OUT=" alias-label C@ .',
            "TLS-EXPORT-RESULT 32 90 FILL",
            "exp-ctx @ test-label 4 0 0 TLS-EXPORT-RESULT 32 TLS-EXPORT",
            '."  INTERNAL=" .',
            '."  INTERNAL-OUT=" TLS-EXPORT-RESULT C@ .',
            "exp-ctx @ TLS-RXW.RECORD 32 90 FILL",
            "exp-ctx @ test-label 4 0 0 exp-ctx @ TLS-RXW.RECORD 32 "
            "TLS-EXPORT",
            '."  RX-INTERNAL=" .',
            '."  RX-INTERNAL-OUT=" exp-ctx @ TLS-RXW.RECORD C@ .',
            "exp-ctx @ test-label 4 0 0 sentinel 8161 TLS-EXPORT",
            '."  TOO-LONG=" .',
            "CREATE context-a 1 ALLOT 97 context-a C!",
            "CREATE context-b 1 ALLOT 98 context-b C!",
            "CREATE out-a 32 ALLOT CREATE out-b 32 ALLOT",
            "exp-ctx @ test-label 4 context-a 1 out-a 32 TLS-EXPORT",
            '."  A=" .',
            "exp-ctx @ test-label 4 context-b 1 out-b 32 TLS-EXPORT",
            '."  B=" .',
            '."  BOUND=" out-a out-b 32 VERIFY .',
            "CREATE fatal-alert 2 ALLOT 2 fatal-alert C! 40 fatal-alert 1+ C!",
            "exp-ctx @ fatal-alert 2 TLS-PROCESS-ALERT DROP",
            ': exp-ms-zero? 32 0 DO DUP I + C@ IF DROP 0 UNLOOP EXIT THEN '
            'LOOP DROP -1 ;',
            '."  FATAL-WIPE=" exp-ctx @ TLS-CTX.EXPORTER-MS exp-ms-zero? .',
            "exp-ctx @ test-label 4 0 0 sentinel 32 TLS-EXPORT",
            '."  CLOSED=" .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("EARLY=-4207 ", text)
        self.assertIn("EARLY-OUT=90 ", text)
        self.assertIn("ZERO=0 ", text)
        self.assertIn("BAD-LABEL=-4208 ", text)
        self.assertIn("ALIAS=-4210 ", text)
        self.assertIn("ALIAS-OUT=97 ", text)
        self.assertIn("INTERNAL=-4210 ", text)
        self.assertIn("INTERNAL-OUT=90 ", text)
        self.assertIn("RX-INTERNAL=-4210 ", text)
        self.assertIn("RX-INTERNAL-OUT=90 ", text)
        self.assertIn("TOO-LONG=-4208 ", text)
        self.assertIn("A=0 ", text)
        self.assertIn("B=0 ", text)
        self.assertIn("BOUND=-1 ", text)
        self.assertIn("FATAL-WIPE=-1 ", text)
        self.assertIn("CLOSED=-4207 ", text)

    def test_tls_owner_recurses_by_task_and_releases_lock(self):
        """Nested TLS helpers retain lock 10 and reject another task."""
        lines = [
            '."  A=" TLS-OWNER-TRY .',
            '."  D1=" TLS-OWNER-DEPTH @ .',
            '."  B=" TLS-OWNER-TRY .',
            '."  D2=" TLS-OWNER-DEPTH @ .',
            "TLS-OWNER-RELEASE",
            '."  D3=" TLS-OWNER-DEPTH @ .',
            "111 _TLSCP-RIP !",
            "TASK-ID 1+ TLS-OWNER-TASK !",
            "TLS-OWNER-RELEASE",
            '."  WRONG-RELEASE=" TLS-OWNER-DEPTH @ .',
            '1 2 3 TLS-CONNECT ."  CONNECT=" .',
            '."  RIP=" _TLSCP-RIP @ .',
            '."  PHASE=" TLS-CONNECT-LAST-ERROR @ .',
            '0 TLS-CTX@ 2 40 TLS-SEND-ALERT-TRY ."  ALERT=" .',
            '."  OTHER=" TLS-OWNER-TRY .',
            '."  HELD=" TLS-OWNER-DEPTH @ .',
            "TASK-ID TLS-OWNER-TASK !",
            "TLS-OWNER-RELEASE",
            '."  D4=" TLS-OWNER-DEPTH @ .',
            '."  RAW=" TLS-OWNER-LOCK SPIN@ .',
            "TLS-OWNER-LOCK SPIN!",
        ]
        text = self._run_kdos(lines)
        self.assertIn("A=0 ", text)
        self.assertIn("D1=1 ", text)
        self.assertIn("B=0 ", text)
        self.assertIn("D2=2 ", text)
        self.assertIn("D3=1 ", text)
        self.assertIn("WRONG-RELEASE=1 ", text)
        self.assertIn("CONNECT=0 ", text)
        self.assertIn("RIP=111 ", text)
        self.assertIn("PHASE=15 ", text)
        self.assertIn("ALERT=-4206 ", text)
        self.assertIn("OTHER=-4206 ", text)
        self.assertIn("HELD=1 ", text)
        self.assertIn("D4=0 ", text)
        self.assertIn("RAW=0 ", text)

    def test_application_schedule_requires_explicit_publication(self):
        """Application secrets are staged before ESTABLISHED is observable."""
        lines = TestKDOSTLSHandshake._TLS_KS_SETUP + [
            "test-ctx @ TLS-KS-HANDSHAKE DROP",
            "1 test-ctx @ TLS-CTX.PEER-AUTH !",
            "TLSS-HANDSHAKE test-ctx @ TLS-CTX.STATE !",
            "TLSH-SERVER-FINISHED test-ctx @ TLS-CTX.HS-STATE !",
            "CREATE app-rec 128 ALLOT",
            "test-ctx @ app-rec TLS-HANDSHAKE-COMPLETE",
            '."  RECLEN=" .',
            '."  PRE-STATE=" test-ctx @ TLS-CTX.STATE @ .',
            '."  PRE-HS=" test-ctx @ TLS-CTX.HS-STATE @ .',
            ': nonzero32? 32 0 DO DUP I + C@ IF DROP -1 UNLOOP EXIT THEN '
            'LOOP DROP 0 ;',
            '."  PRE-EXP=" test-ctx @ TLS-CTX.EXPORTER-MS nonzero32? .',
            'test-ctx @ TLS-HANDSHAKE-PUBLISH ."  PUB=" .',
            '."  POST-STATE=" test-ctx @ TLS-CTX.STATE @ .',
            '."  POST-HS=" test-ctx @ TLS-CTX.HS-STATE @ .',
            '."  POST-EXP=" test-ctx @ TLS-CTX.EXPORTER-MS nonzero32? .',
            '."  HS-WIPE=" test-ctx @ TLS-CTX.HS-SECRET nonzero32? .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("RECLEN=58 ", text)
        self.assertIn("PRE-STATE=1 ", text)
        self.assertIn("PRE-HS=8 ", text)
        self.assertIn("PRE-EXP=-1 ", text)
        self.assertIn("PUB=0 ", text)
        self.assertIn("POST-STATE=2 ", text)
        self.assertIn("POST-HS=9 ", text)
        self.assertIn("POST-EXP=-1 ", text)
        self.assertIn("HS-WIPE=0 ", text)

    def test_server_application_schedule_preserves_handshake_read_epoch(self):
        """Server changes WR first, then RD only after authenticated Finished."""
        lines = TestKDOSTLSHandshake._TLS_KS_SETUP + [
            "TLS-ROLE-SERVER test-ctx @ TLS-CTX.ROLE !",
            "test-ctx @ TLS-KS-HANDSHAKE DROP",
            "CREATE saved-hs-rd 32 ALLOT",
            "CREATE saved-hs-riv 16 ALLOT",
            "CREATE expected-ap-wr 32 ALLOT",
            "CREATE expected-ap-rd 32 ALLOT",
            "test-ctx @ TLS-CTX.RD-KEY saved-hs-rd 32 MOVE",
            "test-ctx @ TLS-CTX.RD-IV saved-hs-riv 16 MOVE",
            "23 test-ctx @ TLS-CTX.RD-SEQ !",
            "TLSS-HANDSHAKE test-ctx @ TLS-CTX.STATE !",
            "TLSH-SERVER-FINISHED test-ctx @ TLS-CTX.HS-STATE !",
            "0 test-ctx @ TLS-CTX.PEER-AUTH !",
            'test-ctx @ TLS-KS-APPLICATION ." KS=" .',
            "test-ctx @ test-ctx @ TLS-CTX.S-AP-TRAFFIC "
            "TLS-L-KEY /TLS-L-KEY 0 0 test-ctx @ TLS-KEY-LEN "
            "expected-ap-wr TLS-EXPAND-LABEL DROP",
            "test-ctx @ test-ctx @ TLS-CTX.C-AP-TRAFFIC "
            "TLS-L-KEY /TLS-L-KEY 0 0 test-ctx @ TLS-KEY-LEN "
            "expected-ap-rd TLS-EXPAND-LABEL DROP",
            '." WR-AP=" test-ctx @ TLS-CTX.WR-KEY '
            "expected-ap-wr 32 _XC-BYTES= .",
            '." RD-HS=" test-ctx @ TLS-CTX.RD-KEY '
            "saved-hs-rd 32 _XC-BYTES= .",
            '." RIV-HS=" test-ctx @ TLS-CTX.RD-IV '
            "saved-hs-riv 16 _XC-BYTES= .",
            '." RS-PENDING=" test-ctx @ TLS-CTX.RD-SEQ @ .',
            '." HS-PENDING=" test-ctx @ TLS-CTX.HS-STATE @ .',
            '." AUTH-PENDING=" test-ctx @ TLS-CTX.PEER-AUTH @ .',
            'test-ctx @ TLS-HANDSHAKE-PUBLISH ." PUB-EARLY=" .',
            'test-ctx @ (TLS-KS-SERVER-READ-APPLICATION) ." CUT=" .',
            '." RD-AP=" test-ctx @ TLS-CTX.RD-KEY '
            "expected-ap-rd 32 _XC-BYTES= .",
            '." RS-READY=" test-ctx @ TLS-CTX.RD-SEQ @ .',
            '." HS-READY=" test-ctx @ TLS-CTX.HS-STATE @ .',
            '." AUTH-READY=" test-ctx @ TLS-CTX.PEER-AUTH @ .',
            'test-ctx @ TLS-HANDSHAKE-PUBLISH ." PUB=" .',
            '." STATE=" test-ctx @ TLS-CTX.STATE @ .',
            '." HS-DONE=" test-ctx @ TLS-CTX.HS-STATE @ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("KS=0 ", text)
        self.assertIn("WR-AP=-1 ", text)
        self.assertIn("RD-HS=-1 ", text)
        self.assertIn("RIV-HS=-1 ", text)
        self.assertIn("RS-PENDING=23 ", text)
        self.assertIn("HS-PENDING=7 ", text)
        self.assertIn("AUTH-PENDING=0 ", text)
        self.assertIn("PUB-EARLY=-4204 ", text)
        self.assertIn("CUT=0 ", text)
        self.assertIn("RD-AP=-1 ", text)
        self.assertIn("RS-READY=0 ", text)
        self.assertIn("HS-READY=8 ", text)
        self.assertIn("AUTH-READY=1 ", text)
        self.assertIn("PUB=0 ", text)
        self.assertIn("STATE=2 ", text)
        self.assertIn("HS-DONE=9 ", text)

    def test_application_schedule_failure_wipes_partial_secrets(self):
        """A failed admitted schedule cannot leave reusable partial keys."""
        lines = TestKDOSTLSHandshake._TLS_KS_SETUP + [
            "test-ctx @ _TKSA-CTX !",
            "test-ctx @ TLS-CTX.WR-KEY 32 90 FILL",
            "test-ctx @ TLS-CTX.RD-KEY 32 90 FILL",
            "test-ctx @ TLS-CTX.EXPORTER-MS 32 90 FILL",
            "test-ctx @ TLS-CTX.HS-SECRET 32 90 FILL",
            "1234 test-ctx @ TLS-CTX.WR-SEQ !",
            "5678 test-ctx @ TLS-CTX.RD-SEQ !",
            "-777 _TKSA-FAIL",
            '."  IOR=" .',
            ': zero32? 32 0 DO DUP I + C@ IF DROP 0 UNLOOP EXIT THEN '
            'LOOP DROP -1 ;',
            '."  WR=" test-ctx @ TLS-CTX.WR-KEY zero32? .',
            '."  RD=" test-ctx @ TLS-CTX.RD-KEY zero32? .',
            '."  EXP=" test-ctx @ TLS-CTX.EXPORTER-MS zero32? .',
            '."  HS=" test-ctx @ TLS-CTX.HS-SECRET zero32? .',
            '."  WSEQ=" test-ctx @ TLS-CTX.WR-SEQ @ .',
            '."  RSEQ=" test-ctx @ TLS-CTX.RD-SEQ @ .',
            '."  ERROR=" test-ctx @ TLS-CTX.ERROR @ .',
            "1 test-ctx @ TLS-CTX.PEER-AUTH !",
            "TLSH-SERVER-FINISHED test-ctx @ TLS-CTX.HS-STATE !",
            'test-ctx @ TLS-KS-APPLICATION ."  RETRY=" .',
            "TLS-ROLE-SERVER test-ctx @ TLS-CTX.ROLE !",
            "0 test-ctx @ TLS-CTX.PEER-AUTH !",
            "TLSH-CLIENT-FINISHED-PENDING test-ctx @ TLS-CTX.HS-STATE !",
            'test-ctx @ (TLS-KS-SERVER-READ-APPLICATION) ."  CUT=" .',
            '."  AUTH2=" test-ctx @ TLS-CTX.PEER-AUTH @ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("IOR=-777 ", text)
        self.assertIn("WR=-1 ", text)
        self.assertIn("RD=-1 ", text)
        self.assertIn("EXP=-1 ", text)
        self.assertIn("HS=-1 ", text)
        self.assertIn("WSEQ=0 ", text)
        self.assertIn("RSEQ=0 ", text)
        self.assertIn("ERROR=-777 ", text)
        self.assertIn("RETRY=-4204 ", text)
        self.assertIn("CUT=-4204 ", text)
        self.assertIn("AUTH2=0 ", text)

    def test_local_fatal_alert_revokes_exporter_without_wire(self):
        """A local fatal decision revokes secrets even under backpressure."""
        exporter_master = (
            "fe22f881176eda18eb8f44529e6792c50c9a3f89452f68d8ae311b4309d3cf50"
        )
        lines = ["TCP-INIT-ALL"] + self._established_context()
        lines += self._forth_bytes("local-exp-master", exporter_master)
        lines += self._forth_bytes("local-label", b"test")
        lines += [
            "local-exp-master exp-ctx @ TLS-CTX.EXPORTER-MS 32 MOVE",
            "0 TCB-N TCB-CLAIM TCPS-ESTABLISHED 0 TCB-N TCB.STATE !",
            "TLS-OWNER-TRY DROP",
            "exp-ctx @ 0 TCB-N _TLS-ATTACH-TCB "
            ">R TLS-OWNER-RELEASE R> ?DUP IF THROW THEN",
            "1 0 TCB-N TCB.SND-NXT ! 0 0 TCB-N TCB.SND-UNA !",
            "exp-ctx @ 2 40 TLS-SEND-ALERT",
            ': local-zero? 32 0 DO DUP I + C@ IF DROP 0 UNLOOP EXIT THEN '
            'LOOP DROP -1 ;',
            '."  STATE=" exp-ctx @ TLS-CTX.STATE @ .',
            '."  AUTH=" exp-ctx @ TLS-CTX.PEER-AUTH @ .',
            '."  ERROR=" exp-ctx @ TLS-CTX.ERROR @ .',
            '."  WIPED=" exp-ctx @ TLS-CTX.EXPORTER-MS local-zero? .',
            "CREATE local-out 32 ALLOT local-out 32 90 FILL",
            "exp-ctx @ local-label 4 0 0 local-out 32 TLS-EXPORT",
            '."  EXPORT=" .',
            '."  OUT=" local-out C@ .',
        ]
        text = self._run_kdos(lines)
        self.assertIn("STATE=3 ", text)
        self.assertIn("AUTH=0 ", text)
        self.assertIn("ERROR=-4212 ", text)
        self.assertIn("WIPED=-1 ", text)
        self.assertIn("EXPORT=-4207 ", text)
        self.assertIn("OUT=90 ", text)


if __name__ == "__main__":
    unittest.main()
