"""KDOS trust anchors and certificate path building: MPTA version 2 trust
bundles sized from need, global and scoped anchors, distrust-after dates,
imposed and certificate name constraints, the backtracking path search and
its one-signature steps, the per-boot cache of verified intermediate edges,
P-384 and RSA-PSS CertificateVerify, and real server chains captured from
public hosts checked against the NSS roots their paths end at.

These classes run on the KDOS + networking snapshot that tests/test_system.py
builds, as their own unit.
"""

import base64
import datetime
import ipaddress
import json
import os
import re
import unittest

from cryptography import x509
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import ec, padding, rsa
from cryptography.x509.oid import ExtendedKeyUsageOID, NameOID

from tests.test_kdos_signatures import (
    PARSE_CERT_WORD,
    mpta_bundle,
    mpta_record,
)
from tests.test_system import (  # noqa: F401  (the harness the classes share)
    PROJECT_ROOT,
    _KDOSNetworkTestBase,
)

FIXTURE = os.path.join(PROJECT_ROOT, "tests", "fixtures", "tls",
                       "webpki_chains.json")

# The captured chains' moment, 2026-11-24T00:00:00Z, when all were valid.
NOW = 1_795_478_400
DAY = 86_400


def _when(seconds: int) -> datetime.datetime:
    return datetime.datetime.fromtimestamp(seconds, datetime.timezone.utc)


def _key(kind: str):
    if kind == "p256":
        return ec.generate_private_key(ec.SECP256R1())
    if kind == "p384":
        return ec.generate_private_key(ec.SECP384R1())
    return rsa.generate_private_key(public_exponent=65537,
                                    key_size=int(kind[3:]))


def _hash_for(key):
    if isinstance(key, ec.EllipticCurvePrivateKey):
        return hashes.SHA384() if key.curve.key_size == 384 else hashes.SHA256()
    return hashes.SHA256()


def _name(common_name: str, *rdns) -> x509.Name:
    attributes = [x509.NameAttribute(oid, value) for oid, value in rdns]
    attributes.append(x509.NameAttribute(NameOID.COMMON_NAME, common_name))
    return x509.Name(attributes)


def make_cert(subject: x509.Name, key, issuer=None, issuer_key=None, *,
              ca=False, path_length=None, dns=(), ips=(), emails=(),
              constraints=None, not_before=NOW - 30 * DAY,
              not_after=NOW + 300 * DAY):
    """A certificate for key, self-signed unless issuer is given."""
    issuer_name = issuer.subject if issuer is not None else subject
    signer = issuer_key if issuer_key is not None else key
    builder = (
        x509.CertificateBuilder()
        .subject_name(subject)
        .issuer_name(issuer_name)
        .public_key(key.public_key())
        .serial_number(x509.random_serial_number())
        .not_valid_before(_when(not_before))
        .not_valid_after(_when(not_after))
        .add_extension(x509.SubjectKeyIdentifier.from_public_key(
            key.public_key()), critical=False)
    )
    if issuer is not None:
        builder = builder.add_extension(
            x509.AuthorityKeyIdentifier.from_issuer_public_key(
                issuer_key.public_key()), critical=False)
    if ca:
        builder = builder.add_extension(
            x509.BasicConstraints(ca=True, path_length=path_length),
            critical=True)
        builder = builder.add_extension(x509.KeyUsage(
            digital_signature=False, content_commitment=False,
            key_encipherment=False, data_encipherment=False,
            key_agreement=False, key_cert_sign=True, crl_sign=True,
            encipher_only=False, decipher_only=False), critical=True)
    else:
        builder = builder.add_extension(
            x509.BasicConstraints(ca=False, path_length=None), critical=True)
        builder = builder.add_extension(x509.KeyUsage(
            digital_signature=True, content_commitment=False,
            key_encipherment=False, data_encipherment=False,
            key_agreement=False, key_cert_sign=False, crl_sign=False,
            encipher_only=False, decipher_only=False), critical=True)
        builder = builder.add_extension(
            x509.ExtendedKeyUsage([ExtendedKeyUsageOID.SERVER_AUTH]),
            critical=False)
    names = ([x509.DNSName(name) for name in dns] +
             [x509.IPAddress(ipaddress.ip_address(ip)) for ip in ips] +
             [x509.RFC822Name(email) for email in emails])
    if names:
        builder = builder.add_extension(
            x509.SubjectAlternativeName(names), critical=False)
    if constraints is not None:
        builder = builder.add_extension(constraints, critical=True)
    return builder.sign(signer, _hash_for(signer))


def der(cert) -> bytes:
    return cert.public_bytes(serialization.Encoding.DER)


def name_constraints_der(constraints: x509.NameConstraints) -> bytes:
    """The DER NameConstraints value a trust record imposes."""
    return constraints.public_bytes()


class _TrustBase(_KDOSNetworkTestBase):
    @staticmethod
    def _bytes(name: str, data: bytes) -> list[str]:
        lines = [f"CREATE {name}"]
        for offset in range(0, len(data), 16):
            lines.append(" ".join(f"{b} C," for b in data[offset:offset + 16]))
        return lines

    def _load(self, bundle: bytes, label="TRUST", name="tv-trust"):
        lines = self._bytes(name, bundle)
        lines.append(f'{name} {len(bundle)} TLS-TRUST-LOAD ." {label}=" .')
        return lines

    def _chain(self, prefix: str, certs) -> list[str]:
        """Parse certs, DER bytes, into descriptors at prefix."""
        lines = []
        for index, data in enumerate(certs):
            lines += self._bytes(f"{prefix}-c{index}", data)
        lines.append(f"CREATE {prefix} /X509-CERT {len(certs)} * ALLOT")
        for index, data in enumerate(certs):
            lines.append(
                f"{prefix}-c{index} {len(data)} {prefix} /X509-CERT {index} * + "
                f'X509-DESC-PARSE ." {prefix.upper()}-P{index}=" .'
            )
        return lines

    @staticmethod
    def _verify(prefix: str, count: int, host: str, label: str,
                now: int = NOW) -> str:
        return (f'{prefix} {count} S" {host}" {now} X509-VERIFY-CHAIN '
                f'." {label}=" .')


class TestKDOSTrustAnchors(_TrustBase):
    """MPTA version 2 bundles: sizing, record checks, scopes and dates."""

    def test_bundle_is_sized_from_its_records(self):
        """Three hundred records load: storage grows to the bundle."""
        root_key = _key("p256")
        root = der(make_cert(_name("Capacity Root"), root_key, ca=True))
        records = [mpta_record(root, f"h{index}.example".encode())
                   for index in range(299)]
        records.append(mpta_record(root))
        big = mpta_bundle(*records, generation=7)
        small = mpta_bundle(mpta_record(root), generation=8)
        lines = self._load(big, "BIG")
        lines += [
            '." COUNT=" TLS-TRUST-COUNT @ .',
            '." VERSION=" TLS-TRUST-VERSION @ .',
            '." GEN=" TLS-TRUST-GENERATION @ .',
            '." LAST-SCOPE=" 299 TLS-TRUST@ TTA-SCOPE-U + @ .',
        ]
        lines += self._load(small, "SMALL", "tv-small")
        lines += [
            '." COUNT2=" TLS-TRUST-COUNT @ .',
            '." GEN2=" TLS-TRUST-GENERATION @ .',
        ]
        text = self._run_kdos(lines, max_steps=4_000_000_000)
        for token in ("BIG=0 ", "COUNT=300 ", "VERSION=2 ", "GEN=7 ",
                      "LAST-SCOPE=0 ", "SMALL=0 ", "COUNT2=1 ", "GEN2=8 "):
            self.assertIn(token, text)

    def test_records_are_checked(self):
        """Version 1, reserved flags, a subdomain flag without a scope, bad
        constraints, a non-CA anchor and truncation all refuse the load."""
        root_key = _key("p256")
        root = der(make_cert(_name("Record Root"), root_key, ca=True))
        leaf = der(make_cert(_name("leaf.example"), _key("p256"),
                             make_cert(_name("Record Root"), root_key, ca=True),
                             root_key, dns=["leaf.example"]))
        good = mpta_bundle(mpta_record(root))
        v1 = b"MPTA\x00\x01" + good[6:]
        cases = {
            "V1": v1,
            "FLAGS": mpta_bundle(mpta_record(root, b"a.example", flags=2)),
            "BARE": mpta_bundle(mpta_record(root, flags=1)),
            "NC": mpta_bundle(mpta_record(root, constraints=b"\x30\x00")),
            "LEAF": mpta_bundle(mpta_record(leaf)),
            "SHORT": good[:-1],
            "EXTRA": good + b"\x00",
        }
        lines = []
        for label, bundle in cases.items():
            lines += self._load(bundle, label, f"tv-{label.lower()}")
            lines.append(f'." {label}-COUNT=" TLS-TRUST-COUNT @ .')
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("V1=-4108 ", text)
        self.assertIn("FLAGS=-4101 ", text)
        self.assertIn("BARE=-4101 ", text)
        self.assertIn("NC=-4101 ", text)
        self.assertIn("LEAF=-4107 ", text)
        self.assertIn("SHORT=-4101 ", text)
        self.assertIn("EXTRA=-4101 ", text)
        for label in cases:
            self.assertIn(f"{label}-COUNT=0 ", text)

    def test_scoped_anchor_binds_its_host(self):
        """A host a scoped anchor names may end only at scoped anchors that
        name it; other hosts end at global anchors."""
        global_key, scoped_key = _key("p256"), _key("p256")
        global_root = make_cert(_name("Global Root"), global_key, ca=True)
        scoped_root = make_cert(_name("Private Root"), scoped_key, ca=True)
        by_global = make_cert(_name("bound.example"), _key("p256"),
                              global_root, global_key,
                              dns=["bound.example", "free.example"])
        by_scoped = make_cert(_name("bound.example"), _key("p256"),
                              scoped_root, scoped_key,
                              dns=["bound.example", "free.example",
                                   "x.sub.example"])
        bundle = mpta_bundle(
            mpta_record(der(global_root)),
            mpta_record(der(scoped_root), b"bound.example"),
            mpta_record(der(scoped_root), b"sub.example", flags=1),
        )
        lines = self._load(bundle)
        lines += self._chain("tv-g", [der(by_global)])
        lines += self._chain("tv-s", [der(by_scoped)])
        lines += [
            self._verify("tv-g", 1, "bound.example", "G-BOUND"),
            self._verify("tv-g", 1, "free.example", "G-FREE"),
            self._verify("tv-s", 1, "bound.example", "S-BOUND"),
            self._verify("tv-s", 1, "free.example", "S-FREE"),
            self._verify("tv-s", 1, "x.sub.example", "S-SUB"),
        ]
        text = self._run_kdos(lines, max_steps=3_000_000_000)
        self.assertIn("TRUST=0 ", text)
        self.assertIn("G-BOUND=-4102 ", text)
        self.assertIn("G-FREE=0 ", text)
        self.assertIn("S-BOUND=0 ", text)
        self.assertIn("S-FREE=-4102 ", text)
        self.assertIn("S-SUB=0 ", text)

    def test_distrust_after_refuses_later_leaves(self):
        """A leaf issued after the root's distrust-after date fails."""
        root_key = _key("p256")
        root = make_cert(_name("Dated Root"), root_key, ca=True)
        leaf = make_cert(_name("dated.example"), _key("p256"), root,
                         root_key, dns=["dated.example"],
                         not_before=NOW - 10 * DAY)
        issued = NOW - 10 * DAY
        lines = self._chain("tv-l", [der(leaf)])
        for label, after in (("BEFORE", issued - 1), ("AT", issued),
                             ("NONE", 0)):
            bundle = mpta_bundle(mpta_record(der(root),
                                             distrust_after=after))
            lines += self._load(bundle, f"T-{label}", f"tv-t-{label.lower()}")
            lines.append(self._verify("tv-l", 1, "dated.example", label))
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("BEFORE=-4111 ", text)
        self.assertIn("AT=0 ", text)
        self.assertIn("NONE=0 ", text)

    def test_imposed_constraints_hold_the_anchor(self):
        """A record's imposed name constraints hold the anchor's subject,
        as NSS holds TUBITAK to .tr."""
        root_key = _key("p256")
        root = make_cert(_name("Held Root"), root_key, ca=True)
        inside = make_cert(_name("a.held.example"), _key("p256"), root,
                           root_key, dns=["a.held.example"])
        outside = make_cert(_name("a.elsewhere.example"), _key("p256"), root,
                            root_key, dns=["a.elsewhere.example"])
        held = name_constraints_der(x509.NameConstraints(
            permitted_subtrees=[x509.DNSName(".held.example")],
            excluded_subtrees=None))
        bundle = mpta_bundle(mpta_record(der(root), constraints=held))
        lines = self._load(bundle)
        lines += self._chain("tv-in", [der(inside)])
        lines += self._chain("tv-out", [der(outside)])
        lines += [
            self._verify("tv-in", 1, "a.held.example", "IN"),
            self._verify("tv-out", 1, "a.elsewhere.example", "OUT"),
        ]
        text = self._run_kdos(lines, max_steps=2_000_000_000)
        self.assertIn("TRUST=0 ", text)
        self.assertIn("IN=0 ", text)
        self.assertIn("OUT=-4107 ", text)


class TestKDOSNameConstraints(_TrustBase):
    """An intermediate's NameConstraints hold every certificate below it."""

    def _hierarchy(self, constraints):
        root_key, ca_key = _key("p256"), _key("p256")
        root = make_cert(_name("NC Root"), root_key, ca=True)
        ca = make_cert(
            _name("NC Intermediate", (NameOID.ORGANIZATION_NAME, "Held Org")),
            ca_key, root, root_key, ca=True, constraints=constraints)
        return root, ca, ca_key

    def _leaf(self, ca, ca_key, subject=None, **names):
        subject = subject or _name("leaf")
        return make_cert(subject, _key("p256"), ca, ca_key, **names)

    def test_dns_ip_and_directory_constraints(self):
        constraints = x509.NameConstraints(
            permitted_subtrees=[
                x509.DNSName("example.com"),
                x509.IPAddress(ipaddress.ip_network("10.0.0.0/8")),
                x509.DirectoryName(x509.Name([x509.NameAttribute(
                    NameOID.ORGANIZATION_NAME, "Held Org")])),
            ],
            excluded_subtrees=[x509.DNSName("bad.example.com")],
        )
        root, ca, ca_key = self._hierarchy(constraints)
        held = lambda cn: _name(cn, (NameOID.ORGANIZATION_NAME, "Held Org"))
        leaves = {
            "OK": self._leaf(ca, ca_key, held("www.example.com"),
                             dns=["www.example.com", "example.com"],
                             ips=["10.1.2.3"]),
            "WILD": self._leaf(ca, ca_key, held("w"),
                               dns=["*.ok.example.com"]),
            "OUTSIDE": self._leaf(ca, ca_key, held("x"),
                                  dns=["www.example.com", "www.example.org"]),
            "EXCLUDED": self._leaf(ca, ca_key, held("b"),
                                   dns=["host.bad.example.com"]),
            "WILD-EXCLUDED": self._leaf(ca, ca_key, held("c"),
                                        dns=["*.example.com"]),
            "IP": self._leaf(ca, ca_key, held("i"), dns=["ip.example.com"],
                             ips=["192.168.1.1"]),
            "DN": self._leaf(ca, ca_key, _name("dn.example.com"),
                             dns=["dn.example.com"]),
        }
        hosts = {"OK": "www.example.com", "WILD": "a.ok.example.com",
                 "OUTSIDE": "www.example.com",
                 "EXCLUDED": "host.bad.example.com",
                 "WILD-EXCLUDED": "a.example.com",
                 "IP": "ip.example.com", "DN": "dn.example.com"}
        lines = self._load(mpta_bundle(mpta_record(der(root))))
        for label, leaf in leaves.items():
            prefix = f"tv-{label.lower()}"
            lines += self._chain(prefix, [der(leaf), der(ca)])
            lines.append(self._verify(prefix, 2, hosts[label], label))
        text = self._run_kdos(lines, max_steps=6_000_000_000)
        self.assertIn("TRUST=0 ", text)
        self.assertIn("OK=0 ", text)
        self.assertIn("WILD=0 ", text)
        for label in ("OUTSIDE", "EXCLUDED", "WILD-EXCLUDED", "IP", "DN"):
            self.assertIn(f"{label}=-4107 ", text, label)

    def test_unchecked_forms_fail_closed(self):
        """A name of a form the verifier does not check fails under a
        subtree of that form; an excluded directory subtree fails every
        named subject."""
        email = x509.NameConstraints(
            permitted_subtrees=[x509.RFC822Name("example.com")],
            excluded_subtrees=None)
        root, ca, ca_key = self._hierarchy(email)
        with_email = self._leaf(ca, ca_key, dns=["mail.example.com"],
                                emails=["a@example.com"])
        without = self._leaf(ca, ca_key, dns=["mail.example.com"])
        directory = x509.NameConstraints(
            permitted_subtrees=None,
            excluded_subtrees=[x509.DirectoryName(x509.Name([
                x509.NameAttribute(NameOID.ORGANIZATION_NAME, "Other")]))])
        root2, ca2, ca2_key = self._hierarchy(directory)
        named = self._leaf(ca2, ca2_key, dns=["dir.example.com"])
        lines = self._load(mpta_bundle(mpta_record(der(root)),
                                       mpta_record(der(root2))))
        lines += self._chain("tv-e", [der(with_email), der(ca)])
        lines += self._chain("tv-n", [der(without), der(ca)])
        lines += self._chain("tv-d", [der(named), der(ca2)])
        lines += [
            self._verify("tv-e", 2, "mail.example.com", "EMAIL"),
            self._verify("tv-n", 2, "mail.example.com", "NOEMAIL"),
            self._verify("tv-d", 2, "dir.example.com", "DIR"),
        ]
        text = self._run_kdos(lines, max_steps=4_000_000_000)
        self.assertIn("EMAIL=-4107 ", text)
        self.assertIn("NOEMAIL=0 ", text)
        self.assertIn("DIR=-4107 ", text)


class TestKDOSPathSearch(_TrustBase):
    """Backtracking, one signature per step, and the edge cache."""

    def test_backtracks_past_an_impostor_issuer(self):
        """A presented CA with the right name and the wrong key is tried and
        abandoned for the real issuer."""
        root_key, ca_key, fake_key = _key("p256"), _key("p256"), _key("p256")
        root = make_cert(_name("BT Root"), root_key, ca=True)
        ca = make_cert(_name("BT CA"), ca_key, root, root_key, ca=True)
        impostor = make_cert(_name("BT CA"), fake_key, root, root_key,
                             ca=True)
        leaf = make_cert(_name("bt.example"), _key("p256"), ca, ca_key,
                         dns=["bt.example"])
        lines = self._load(mpta_bundle(mpta_record(der(root))))
        # Without key identifiers the impostor links by name alone.
        lines += self._chain("tv-p", [der(leaf), der(impostor), der(ca)])
        lines += [
            "tv-p XC.AKI-U + 0 SWAP !",
            "tv-p /X509-CERT + XC.SKI-U + 0 SWAP !",
            "tv-p /X509-CERT 2 * + XC.SKI-U + 0 SWAP !",
            self._verify("tv-p", 3, "bt.example", "PATH"),
            self._verify("tv-p", 2, "bt.example", "IMPOSTOR-ONLY"),
        ]
        text = self._run_kdos(lines, max_steps=3_000_000_000)
        self.assertIn("PATH=0 ", text)
        self.assertIn("IMPOSTOR-ONLY=-4106 ", text)

    def test_steps_check_one_signature_and_reuse_verified_edges(self):
        """Each step checks at most one signature; a second search over the
        same intermediate takes its edge from the cache."""
        root_key, ca_key = _key("p384"), _key("p384")
        root = make_cert(_name("Step Root"), root_key, ca=True)
        ca = make_cert(_name("Step CA"), ca_key, root, root_key, ca=True)
        leaf = make_cert(_name("step.example"), _key("p256"), ca, ca_key,
                         dns=["step.example"])
        lines = self._load(mpta_bundle(mpta_record(der(root))))
        lines += self._chain("tv-s", [der(leaf), der(ca)])
        lines += [
            "VARIABLE tv-t0",
            ": tv-step ( -- ior cycles )",
            "  PERF-CYCLES tv-t0 ! X509-PATH-STEP PERF-CYCLES tv-t0 @ - ;",
            ": tv-search ( -- )",
            f'  tv-s 2 S" step.example" {NOW} X509-PATH-BEGIN ." B=" .',
            '  tv-step ." S1=" SWAP . ." C1=" .',
            '  tv-step ." S2=" SWAP . ." C2=" .',
            '  tv-step ." S3=" SWAP . ." C3=" . ;',
            "X509-EDGE-CACHE-CLEAR",
            '." FIRST:" tv-search CR',
            '." SECOND:" tv-search CR',
        ]
        text = self._run_kdos(lines, max_steps=3_000_000_000)
        first = text[text.index("FIRST:"):text.index("SECOND:")]
        second = text[text.index("SECOND:"):]
        cycles = {}
        for name, part in (("first", first), ("second", second)):
            self.assertIn("B=1 ", part)
            self.assertIn("S1=1 ", part)
            self.assertIn("S2=0 ", part)
            self.assertIn("S3=0 ", part)
            cycles[name] = [int(re.search(rf"C{i}=(\d+)", part).group(1))
                            for i in (1, 2)]
        # The first search's second step checks the P-384 intermediate
        # signature; the second search finds that edge in the cache.
        self.assertGreater(cycles["first"][1], 20_000_000)
        self.assertLess(cycles["second"][1], 2_000_000)
        self.assertGreater(cycles["second"][0], 20_000_000)

    def test_certificate_message_pends_until_its_path_is_checked(self):
        """TLS-PROCESS-HS-MSG answers X509-PATH-PENDING while the path
        search runs, appends the message to the transcript once, and then
        moves on."""
        root_key, ca_key = _key("p256"), _key("p256")
        root = make_cert(_name("HS Root"), root_key, ca=True)
        ca = make_cert(_name("HS CA"), ca_key, root, root_key, ca=True)
        leaf = make_cert(_name("hs.example"), _key("p256"), ca, ca_key,
                         dns=["hs.example"])
        entries = b"".join(len(c).to_bytes(3, "big") + c + b"\x00\x00"
                           for c in (der(leaf), der(ca)))
        body = b"\x00" + len(entries).to_bytes(3, "big") + entries
        msg = b"\x0b" + len(body).to_bytes(3, "big") + body
        epoch_ms = NOW * 1000
        rtc = 0xFFFF_FF00_0000_0B08
        lines = self._load(mpta_bundle(mpta_record(der(root))))
        lines += self._bytes("tv-msg", msg)
        lines += [f"{(epoch_ms >> (8 * i)) & 0xFF} 0x{rtc + i:016X} C!"
                  for i in range(8)]
        lines += [
            'S" hs.example" TLS-SNI-HOST SWAP CMOVE',
            "10 TLS-SNI-LEN !",
            "VARIABLE tv-ctx 0 TLS-CTX@ tv-ctx !",
            "TLS-ROLE-CLIENT tv-ctx @ TLS-CTX.ROLE !",
            "TLSS-HANDSHAKE tv-ctx @ TLS-CTX.STATE !",
            "TLSH-EE-RCVD tv-ctx @ TLS-CTX.HS-STATE !",
            "X509-EDGE-CACHE-CLEAR",
            "TLS-TR-RESET",
            f"tv-ctx @ tv-msg {len(msg)} TLS-PROCESS-HS-MSG",
            '." R1=" . ." TR1=" TLS-HS-TR-LEN @ .',
            '." ST1=" tv-ctx @ TLS-CTX.HS-STATE @ .',
            f"tv-ctx @ tv-msg {len(msg)} TLS-PROCESS-HS-MSG",
            '." R2=" . ." TR2=" TLS-HS-TR-LEN @ .',
            '." ST2=" tv-ctx @ TLS-CTX.HS-STATE @ .',
            '." KEY=" _TLS-SERVER-PUBKEY-ALGO @ .',
        ]
        text = self._run_kdos(lines, max_steps=3_000_000_000)
        self.assertIn("TRUST=0 ", text)
        self.assertIn("R1=1 TR1=0 ", text)
        self.assertIn("ST1=3 ", text)       # TLSH-EE-RCVD, still waiting
        self.assertIn(f"R2=0 TR2={len(msg)} ", text)
        self.assertIn("ST2=4 ", text)       # TLSH-CERT-RCVD
        self.assertIn("KEY=2 ", text)

    def test_new_client_hello_abandons_a_pending_search(self):
        """A search left pending by an abandoned handshake never finishes
        over the next handshake's Certificate message, even one of the
        same length in the same place: that message starts a new search,
        which checks its own leaf."""
        root_key, ca_key = _key("p256"), _key("p256")
        root = make_cert(_name("HS Root"), root_key, ca=True)
        ca = make_cert(_name("HS CA"), ca_key, root, root_key, ca=True)
        first = make_cert(_name("hs.example"), _key("p256"), ca, ca_key,
                          dns=["hs.example"])
        # A leaf for another host, of the same length, so that the second
        # message lands exactly over the first.
        for _ in range(64):
            other = make_cert(_name("hx.example"), _key("p256"), ca, ca_key,
                              dns=["hx.example"])
            if len(der(other)) == len(der(first)):
                break
        self.assertEqual(len(der(other)), len(der(first)))

        def message(leaf) -> bytes:
            entries = b"".join(len(c).to_bytes(3, "big") + c + b"\x00\x00"
                               for c in (der(leaf), der(ca)))
            body = b"\x00" + len(entries).to_bytes(3, "big") + entries
            return b"\x0b" + len(body).to_bytes(3, "big") + body

        msg, msg2 = message(first), message(other)
        epoch_ms = NOW * 1000
        rtc = 0xFFFF_FF00_0000_0B08
        lines = self._load(mpta_bundle(mpta_record(der(root))))
        lines += self._bytes("tv-msg", msg)
        lines += self._bytes("tv-msg2", msg2)
        lines += [f"{(epoch_ms >> (8 * i)) & 0xFF} 0x{rtc + i:016X} C!"
                  for i in range(8)]
        lines += [
            'S" hs.example" TLS-SNI-HOST SWAP CMOVE',
            "10 TLS-SNI-LEN !",
            "VARIABLE tv-ctx 0 TLS-CTX@ tv-ctx !",
            "TLS-ROLE-CLIENT tv-ctx @ TLS-CTX.ROLE !",
            "TLSS-HANDSHAKE tv-ctx @ TLS-CTX.STATE !",
            "TLSH-EE-RCVD tv-ctx @ TLS-CTX.HS-STATE !",
            "X509-EDGE-CACHE-CLEAR",
            "TLS-TR-RESET",
            f"tv-ctx @ tv-msg {len(msg)} TLS-PROCESS-HS-MSG",
            '." R1=" .',
            # The handshake is abandoned; the next one starts on the same
            # context, and its Certificate message lands where the first
            # one was.
            "TLSS-NONE tv-ctx @ TLS-CTX.STATE !",
            'tv-ctx @ TLS-BUILD-CLIENT-HELLO NIP 0<> ." CH=" .',
            "TLSH-EE-RCVD tv-ctx @ TLS-CTX.HS-STATE !",
            "TLS-TR-RESET",
            f"tv-msg2 tv-msg {len(msg)} CMOVE",
            f"tv-ctx @ tv-msg {len(msg)} TLS-PROCESS-HS-MSG",
            '." R2=" . ." HOST=" TLS-CERT-HOSTNAME .',
            '." ST2=" tv-ctx @ TLS-CTX.HS-STATE @ .',
        ]
        text = self._run_kdos(lines, max_steps=3_000_000_000)
        self.assertIn("TRUST=0 ", text)
        self.assertIn("R1=1 ", text)        # the first search is pending
        self.assertIn("CH=-1 ", text)
        host = int(re.search(r"HOST=(-?\d+)", text).group(1))
        self.assertIn(f"R2={host} ", text)  # the new leaf names another host
        self.assertIn("ST2=3 ", text)       # TLSH-EE-RCVD, not accepted


class TestKDOSCertificateVerifySchemes(_TrustBase):
    """CertificateVerify with P-384/SHA-384 and RSA-PSS with SHA-384."""

    CONTEXT = b"TLS 1.3, server CertificateVerify"

    def _cv_lines(self, scheme: int, signature: bytes, transcript: bytes,
                  key_lines: list[str]) -> list[str]:
        body = (scheme.to_bytes(2, "big") +
                len(signature).to_bytes(2, "big") + signature)
        message = b"\x0f" + len(body).to_bytes(3, "big") + body
        lines = key_lines
        lines += self._bytes("tv-cv", message)
        lines += self._bytes("tv-tr", transcript)
        lines += [
            "VARIABLE tv-ctx 0 TLS-CTX@ tv-ctx !",
            "TLS-ROLE-CLIENT tv-ctx @ TLS-CTX.ROLE !",
            "TLS-HASH-SHA256 tv-ctx @ TLS-CTX.HASH-ID !",
            "TLS-SUITE-AES128-SHA256 tv-ctx @ TLS-CTX.SUITE !",
            "TLS-TR-RESET",
            f"tv-tr {len(transcript)} TLS-TR-APPEND",
            f'tv-ctx @ tv-cv {len(message)} TLS-VERIFY-CERT-SIG ." CV=" .',
            "tv-cv 20 + DUP C@ 1 XOR SWAP C!",
            f'tv-ctx @ tv-cv {len(message)} TLS-VERIFY-CERT-SIG ." BAD=" .',
        ]
        return lines

    def _content(self, transcript: bytes) -> bytes:
        import hashlib
        return (b"\x20" * 64 + self.CONTEXT + b"\x00" +
                hashlib.sha256(transcript).digest())

    def test_p384_sha384(self):
        key = _key("p384")
        transcript = b"p384 transcript"
        signature = key.sign(self._content(transcript),
                             ec.ECDSA(hashes.SHA384()))
        point = key.public_key().public_bytes(
            serialization.Encoding.X962,
            serialization.PublicFormat.UncompressedPoint)
        key_lines = self._bytes("tv-point", point) + [
            "tv-point _TLS-SERVER-PUBKEY 97 CMOVE",
            "97 _TLS-SERVER-PUBKEY-LEN !",
            "X509-KEY-P384 _TLS-SERVER-PUBKEY-ALGO !",
        ]
        text = self._run_kdos(self._cv_lines(0x0503, signature, transcript,
                                             key_lines),
                              max_steps=2_000_000_000)
        self.assertIn("CV=0 ", text)
        self.assertIn("BAD=-1 ", text)

    def test_rsa_pss_sha384_with_a_3072_bit_key(self):
        key = _key("rsa3072")
        transcript = b"rsa transcript"
        signature = key.sign(
            self._content(transcript),
            padding.PSS(mgf=padding.MGF1(hashes.SHA384()), salt_length=48),
            hashes.SHA384())
        numbers = key.public_key().public_numbers()
        modulus = numbers.n.to_bytes(384, "big")
        key_lines = self._bytes("tv-mod", modulus) + [
            "tv-mod _TLS-SERVER-PUBKEY 384 CMOVE",
            "384 _TLS-SERVER-PUBKEY-LEN !",
            "X509-KEY-RSA _TLS-SERVER-PUBKEY-ALGO !",
            "RSA-E-65537 _TLS-SERVER-PUBKEY-E 3 CMOVE",
            "3 _TLS-SERVER-PUBKEY-E-U !",
        ]
        text = self._run_kdos(self._cv_lines(0x0805, signature, transcript,
                                             key_lines),
                              max_steps=2_000_000_000)
        self.assertIn("CV=0 ", text)
        self.assertIn("BAD=-1 ", text)


class TestKDOSWebPKIChains(_TrustBase):
    """Chains captured from public hosts verify against their NSS roots,
    including P-384 and RSA-4096 signers and cross-signed tops whose own
    issuer is not loaded."""

    @classmethod
    def setUpClass(cls):
        super().setUpClass()
        with open(FIXTURE, encoding="utf-8") as stream:
            cls.FIX = json.load(stream)

    def _bundle(self) -> bytes:
        return mpta_bundle(*(mpta_record(base64.b64decode(der))
                             for der in self.FIX["roots"].values()))

    def _check_hosts(self, hosts):
        bundle = self._bundle()
        lines = self._load(bundle)
        lines += ["VARIABLE tv-t0"]
        for index, entry in enumerate(self.FIX["hosts"]):
            if entry["host"] not in hosts:
                continue
            chain = [base64.b64decode(c) for c in entry["chain"]]
            prefix = f"tv-h{index}"
            lines += self._chain(prefix, chain)
            lines += [
                "PERF-CYCLES tv-t0 !",
                f'{prefix} {len(chain)} S" {entry["host"]}" '
                f'{self.FIX["now"]} X509-VERIFY-CHAIN',
                f'." H{index}=" . ." K{index}=" PERF-CYCLES tv-t0 @ - .',
            ]
        text = self._run_kdos(lines, max_steps=12_000_000_000)
        self.assertIn("TRUST=0 ", text)
        results = {}
        for index, entry in enumerate(self.FIX["hosts"]):
            if entry["host"] not in hosts:
                continue
            match = re.search(rf"H{index}=(-?\d+) K{index}=(\d+)", text)
            self.assertIsNotNone(match, entry["host"])
            results[entry["host"]] = (int(match.group(1)), int(match.group(2)))
        for host, (ior, cycles) in results.items():
            print(f"  {host}: ior {ior}, {cycles:,} cycles")
            self.assertEqual(ior, 0, host)
        return results

    def test_ecdsa_chains(self):
        self._check_hosts({"github.com", "valid-isrgrootx2.letsencrypt.org",
                           "www.google.com", "feeds.bbci.co.uk",
                           "letsencrypt.org", "auth.openai.com"})

    def test_rsa_chains(self):
        self._check_hosts({"www.cbsnews.com", "hnrss.org", "rss.nytimes.com",
                           "www.theguardian.com", "public.api.bsky.app",
                           "bsky.social"})

    def test_wrong_host_and_missing_root_fail(self):
        entry = next(e for e in self.FIX["hosts"] if e["host"] == "github.com")
        chain = [base64.b64decode(c) for c in entry["chain"]]
        others = mpta_bundle(*(mpta_record(base64.b64decode(der))
                               for label, der in self.FIX["roots"].items()
                               if label not in entry["roots"]))
        lines = self._load(self._bundle())
        lines += self._chain("tv-gh", chain)
        lines.append(self._verify("tv-gh", len(chain), "gitlab.com", "HOST",
                                  self.FIX["now"]))
        lines += self._load(others, "OTHERS", "tv-others")
        lines.append(self._verify("tv-gh", len(chain), "github.com", "NOROOT",
                                  self.FIX["now"]))
        text = self._run_kdos(lines, max_steps=4_000_000_000)
        self.assertIn("HOST=-4105 ", text)
        self.assertIn("OTHERS=0 ", text)
        self.assertIn("NOROOT=-4102 ", text)


if __name__ == "__main__":
    unittest.main()
