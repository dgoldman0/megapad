# Megapad-64 — Build & Test
# ==========================
#
# PyPy gives ~5× speedup on the CPU emulator loop; xdist adds parallelism.
#
#   make test           PyPy + 8 xdist workers  (~4 min for 1183 tests)
#   make test-seq       PyPy sequential          (~8 min)
#   make test-cpython   CPython fallback         (~40 min)
#   make test-quick     PyPy, BIOS+CPU only      (~6 sec)
#
# PyPy setup (one-time):
#   make setup-pypy

PYPY     := .pypy/bin/pypy3
CPYTHON  := python
PYTEST   := -m pytest test_system.py test_megapad64.py
WORKERS  := 8

# --- Primary test target: PyPy + xdist ---
.PHONY: test
test: check-pypy
	$(PYPY) $(PYTEST) -n $(WORKERS)

# --- PyPy sequential (no xdist overhead, good for debugging) ---
.PHONY: test-seq
test-seq: check-pypy
	$(PYPY) $(PYTEST)

# --- CPython fallback ---
.PHONY: test-cpython
test-cpython:
	$(CPYTHON) $(PYTEST)

# --- Quick smoke test: BIOS + CPU only (~6s under PyPy) ---
.PHONY: test-quick
test-quick: check-pypy
	$(PYPY) $(PYTEST) -k "TestBIOS and not test_autoboot or TestMulticore" --tb=short

# --- Single test (usage: make test-one K=test_coreid_word) ---
.PHONY: test-one
test-one: check-pypy
	$(PYPY) $(PYTEST) -k "$(K)" --tb=long -v

# --- Run the interactive emulator ---
.PHONY: run
run:
	$(CPYTHON) cli.py --bios bios.asm --forth kdos.f

# --- PyPy setup ---
.PHONY: setup-pypy
setup-pypy:
	@if [ ! -f $(PYPY) ]; then \
		echo "Downloading PyPy 3.10 ..."; \
		curl -sL https://downloads.python.org/pypy/pypy3.10-v7.3.17-linux64.tar.bz2 | tar xj -C /tmp; \
		mkdir -p .pypy; \
		cp -r /tmp/pypy3.10-v7.3.17-linux64/* .pypy/; \
		rm -rf /tmp/pypy3.10-v7.3.17-linux64; \
		$(PYPY) -m ensurepip -q; \
		$(PYPY) -m pip install pytest pytest-xdist -q; \
		echo "PyPy installed at .pypy/"; \
	else \
		echo "PyPy already installed."; \
	fi

.PHONY: check-pypy
check-pypy:
	@if [ ! -f $(PYPY) ]; then \
		echo "ERROR: PyPy not found. Run 'make setup-pypy' first."; \
		exit 1; \
	fi
