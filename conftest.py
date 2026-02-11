"""
Pytest configuration for Megapad-64 test suite.

Supports parallel execution via pytest-xdist:
    pytest -n auto          # auto-detect CPU count
    pytest -n 4             # use 4 workers
    pytest -n auto --dist loadgroup  # group by @pytest.mark.xdist_group

Without -n, tests run sequentially as before.
"""
