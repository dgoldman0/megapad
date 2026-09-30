"""Composition helpers for the declared hybrid ABI.

Only declaration loading is available in this implementation slice. Importing
this package does not load either engine, create a runtime, or enable a mode.
"""

from hybrid.manifest import HybridManifestError, load_manifest_v1

__all__ = ["HybridManifestError", "load_manifest_v1"]
