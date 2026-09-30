"""Composition helpers for the declared hybrid ABI.

Importing this package or loading a manifest does not select an engine. Import
HybridRuntime from hybrid.runtime and HybridSession from hybrid.session when
constructing a bounded integer-routine session.
"""

from hybrid.manifest import (
    HybridManifestError,
    load_manifest,
    load_manifest_v1,
    load_manifest_v2,
)

__all__ = ["HybridManifestError", "load_manifest", "load_manifest_v1", "load_manifest_v2"]
