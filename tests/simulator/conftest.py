from __future__ import annotations

import pytest


def pytest_configure(config):
    config.addinivalue_line(
        "markers",
        "python_dictionary_only: a white-box test of the Python reference "
        "dictionary's private state or memory calls",
    )


@pytest.fixture(params=["python", "native"])
def dictionary_backend(request, monkeypatch):
    """Run a dictionary test against both implementations.

    The Python ``Dictionary`` is the reference.  The native backend's
    ``NativeDictionary`` must match it exactly, so a module that opts in sees
    its ``Dictionary`` name bound to each in turn.
    """

    if request.param == "native":
        if request.node.get_closest_marker("python_dictionary_only"):
            pytest.skip("white-box test of the Python reference dictionary")
        pytest.importorskip("_megaforth_native")
        from simulator.native_dictionary import NativeDictionary

        monkeypatch.setattr(request.module, "Dictionary", NativeDictionary)
    return request.param
