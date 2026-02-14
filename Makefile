# Megapad-64 — Build & Test
# ==========================
#
# The C++ accelerator (~50× faster than CPython, ~10× faster than PyPy)
# is the DEFAULT test backend.  All targets auto-build it.
#
# NEVER run `python -m pytest` directly — conftest.py will block it.
# Always use these Makefile targets.
#
#   make test           Background + live dashboard   (DEFAULT, ~1 min)
#   make test K=X       Background subset
#   make test-status    One-shot progress dashboard
#   make test-watch     Auto-refresh dashboard every 5s
#   make test-failures  Show only failures
#   make test-kill      Kill stuck background run
#
# Foreground (for debugging / CI):
#   make test-fg        C++ accel + 8 xdist workers, foreground
#   make test-seq       C++ accel sequential          (~3 min)
#   make test-one K=X   Single test/class, accel      (foreground)
#   make test-quick     Quick BIOS+CPU smoke test     (~3 sec)
#
# Fallback targets (non-accelerated):
#   make test-pypy      PyPy + 8 workers (no C++ accel)
#   make test-cpython   CPython fallback (~40 min, no accel)
#
# Real-network tests (requires TAP — see test_networking.py):
#   make test-net       All real-net tests against TAP device
#   make test-net K=X   Subset of real-net tests
#
# conftest.py writes live status to /tmp/megapad_test_status.json.
# test_monitor.py reads it and renders the dashboard.
#
# PyPy setup (one-time):
#   make setup-pypy

PYPY     := .pypy/bin/pypy3
CPYTHON  := python
VENV_PY  := .venv/bin/python
PYTEST   := -m pytest test_system.py test_megapad64.py test_networking.py
PYTEST_SIM := -m pytest test_system.py test_megapad64.py test_networking.py -m "not realnet"
WORKERS  := 8

# --- C++ accelerator ---
.PHONY: accel accel-clean
accel:
	$(VENV_PY) setup_accel.py build_ext --inplace

accel-clean:
	rm -rf build/ _mp64_accel*.so

# --- Accelerated test run: CPython + C++ extension ---
.PHONY: test-accel
test-accel: accel
	MP64_VIA_MAKE=1 $(VENV_PY) $(PYTEST)

# --- Benchmark: compare Python vs C++ ---
.PHONY: bench
bench: accel
	$(VENV_PY) bench_accel.py

# --- Primary test target: background + live dashboard (DEFAULT) ---
.PHONY: test
test: test-bg

# --- Foreground: C++ accel + xdist ---
.PHONY: test-fg
test-fg: accel
	MP64_VIA_MAKE=1 $(VENV_PY) $(PYTEST_SIM) -n $(WORKERS)

# --- Sequential (C++ accel, no xdist overhead, good for debugging) ---
.PHONY: test-seq
test-seq: accel
	MP64_VIA_MAKE=1 $(VENV_PY) $(PYTEST_SIM)

# --- PyPy fallback (no C++ accel, JIT instead) ---
.PHONY: test-pypy
test-pypy: check-pypy
	MP64_VIA_MAKE=1 $(PYPY) $(PYTEST) -n $(WORKERS)

# --- CPython fallback ---
.PHONY: test-cpython
test-cpython:
	MP64_VIA_MAKE=1 $(CPYTHON) $(PYTEST)

# --- Quick smoke test: BIOS + CPU only ---
.PHONY: test-quick
test-quick: accel
	MP64_VIA_MAKE=1 $(VENV_PY) $(PYTEST_SIM) -k "TestBIOS and not test_autoboot or TestMulticore" --tb=short

# --- Single test (usage: make test-one K=test_coreid_word) ---
.PHONY: test-one
test-one: accel
	MP64_VIA_MAKE=1 $(VENV_PY) $(PYTEST) -k "$(K)" --tb=long -v

# --- Single test with C++ accelerator (alias for test-one) ---
.PHONY: test-one-accel
test-one-accel: test-one

# --- Background test run with live monitoring ---
# Usage: make test-bg          (full suite)
#        make test-bg K=TestFoo (subset)
# Then:  make test-status  or  make test-watch
.PHONY: test-bg
test-bg: accel
	@if [ -f /tmp/megapad_test_pid.txt ] && kill -0 $$(cat /tmp/megapad_test_pid.txt) 2>/dev/null; then \
		echo "Tests already running (PID $$(cat /tmp/megapad_test_pid.txt)). Use 'make test-kill' first."; \
		exit 1; \
	fi
	@rm -f /tmp/megapad_test_status.json /tmp/megapad_test_pid.txt
	@echo "Starting tests in background (C++ accel)..."
	@if [ -n "$(K)" ]; then \
		nohup env MP64_VIA_MAKE=1 $(VENV_PY) $(PYTEST_SIM) -n $(WORKERS) --tb=long -k "$(K)" \
			> /tmp/megapad_test_output.txt 2>&1 & \
		echo "$$!" > /tmp/megapad_test_pid.txt; \
	else \
		nohup env MP64_VIA_MAKE=1 $(VENV_PY) $(PYTEST_SIM) -n $(WORKERS) --tb=long \
			> /tmp/megapad_test_output.txt 2>&1 & \
		echo "$$!" > /tmp/megapad_test_pid.txt; \
	fi
	@echo "PID: $$(cat /tmp/megapad_test_pid.txt)"
	@echo "Monitor: make test-status  |  make test-watch"

# --- Real-network tests (requires TAP device) ---
# Usage: make test-net          (all real-net tests)
#        make test-net K=TestRealNetARP
.PHONY: test-net
test-net: accel
	@echo "Running real-network tests (TAP: $${MP64_TAP:-mp64tap0})..."
	MP64_VIA_MAKE=1 $(VENV_PY) -m pytest test_networking.py -v --tb=long $(if $(K),-k "$(K)",)

# --- Show live test status ---
.PHONY: test-status
test-status:
	@python3 test_monitor.py

# --- Auto-refresh test status ---
.PHONY: test-watch
test-watch:
	@python3 test_monitor.py --watch 5

# --- Show only test failures ---
.PHONY: test-failures
test-failures:
	@python3 test_monitor.py --failures

# --- Kill background test run ---
.PHONY: test-kill
test-kill:
	@if [ -f /tmp/megapad_test_pid.txt ]; then \
		PID=$$(cat /tmp/megapad_test_pid.txt); \
		if kill -0 $$PID 2>/dev/null; then \
			kill -- -$$PID 2>/dev/null || kill $$PID 2>/dev/null; \
			rm -f /tmp/megapad_test_pid.txt; \
			echo "Killed PID $$PID."; \
		else \
			rm -f /tmp/megapad_test_pid.txt; \
			echo "Process already exited."; \
		fi; \
	else \
		echo "No tests running (no PID file)."; \
	fi

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
