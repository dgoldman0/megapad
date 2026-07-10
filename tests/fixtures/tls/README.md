# Native TLS certificate fixtures

This deterministic P-256 chain is test data only. The root, intermediate, and
server certificates use fixed test keys, SHA-256 signatures, explicit CA and
key-usage constraints, serverAuth EKU, SAN names, and 2024-2030+ validity.

The `.der.b64` files are base64-encoded DER so they remain reviewable text.
Their public keys correspond to deliberately trivial test scalars (root 1,
intermediate 2, leaf 3); no private-key file is needed. Never install this root
or any certificate from this directory in a product trust bundle.
