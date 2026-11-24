# Native TLS certificate fixtures

This deterministic P-256 chain is test data only. The root, intermediate, and
server certificates use fixed test keys, SHA-256 signatures, explicit CA and
key-usage constraints, serverAuth EKU, SAN names, and 2024-2030+ validity.

The `.der.b64` files are base64-encoded DER so they remain reviewable text.
Their public keys correspond to deliberately trivial test scalars (root 1,
intermediate 2, leaf 3); no private-key file is needed. Never install this root
or any certificate from this directory in a product trust bundle.

`signature_vectors.json` holds RSA public keys at 2048, 3072 and 4096 bits
with PKCS#1 v1.5 and PSS signatures over SHA-256, SHA-384 and SHA-512
digests, and P-384 public keys with ECDSA signatures over SHA-384, SHA-256
and SHA-512 digests. Python's `cryptography` 45.0.5 made them on 2026-11-24
from freshly generated keys whose private halves were discarded, so the
vectors come from an implementation independent of KDOS.
