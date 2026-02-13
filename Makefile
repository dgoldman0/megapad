# Megapad-64 — Build & Test
# ==========================
#
# PyPy gives ~5× speedup on the CPU emulator loop; xdist adds parallelism.
#
#   make test           PyPy + 8 xdist workers  (~4 min for full suite)
#   make test-seq       PyPy sequential          (~8 min)
#   make test-cpython   CPython fallback         (~40 min)
#   make test-quick     PyPy, BIOS+CPU only      (~6 sec)
#   make test-one K=X   Single test/class under PyPy (foreground)
#
# Background + Live Monitoring (preferred for long runs):
#   make test-bg        Launch full suite in background
#   make test-bg K=X    Launch subset in background
#   make test-status    One-shot progress dashboard
#   make test-watch     Auto-refresh dashboard every 5s
#   make test-failures  Show only failures
#   make test-kill      Kill stuck background run
#
# conftest.py writes live status to /tmp/megapad_test_status.json.
# test_monitor.py reads it and renders the dashboard.
#
# NEVER run `python -m pytest` directly — always use these targets.
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

# --- Background test run with live monitoring ---
# Usage: make test-bg          (full suite)
#        make test-bg K=TestFoo (subset)
# Then:  make test-status  or  make test-watch
.PHONY: test-bg
test-bg: check-pypy
	@if [ -f /tmp/megapad_test_pid.txt ] && kill -0 $$(cat /tmp/megapad_test_pid.txt) 2>/dev/null; then \
		echo "Tests already running (PID $$(cat /tmp/megapad_test_pid.txt)). Use 'make test-kill' first."; \
		exit 1; \
	fi
	@rm -f /tmp/megapad_test_status.json /tmp/megapad_test_pid.txt
	@echo "Starting tests in background..."
	@if [ -n "$(K)" ]; then \
		nohup $(PYPY) $(PYTEST) -n $(WORKERS) --tb=long -k "$(K)" \
			> /tmp/megapad_test_output.txt 2>&1 & \
		echo "$$!" > /tmp/megapad_test_pid.txt; \
	else \
		nohup $(PYPY) $(PYTEST) -n $(WORKERS) --tb=long \
			> /tmp/megapad_test_output.txt 2>&1 & \
		echo "$$!" > /tmp/megapad_test_pid.txt; \
	fi
	@echo "PID: $$(cat /tmp/megapad_test_pid.txt)"
	@echo "Monitor: make test-status  |  make test-watch"

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
