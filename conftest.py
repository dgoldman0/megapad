"""
Pytest configuration for Megapad-64 test suite.

Recommended: run tests with PyPy for ~5× speedup on CPU emulation.
    make test               # PyPy + 8 xdist workers (~4 min)
    make test-seq           # PyPy sequential (~8 min)
    make test-cpython       # CPython fallback (~40 min)

For parallel execution with xdist:
    pypy3 -m pytest -n 8            # 8 workers
    pypy3 -m pytest -n auto         # auto-detect CPU count

Without -n, tests run sequentially.
"""
