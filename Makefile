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
#   make test-one K=X   Background single test/class + monitoring
#   make test-status    One-shot progress dashboard
#   make test-watch     Auto-refresh dashboard every 5s
#   make test-failures  Show only failures
#   make test-kill      Kill stuck background run
#   make test-quick     Quick BIOS+CPU smoke test     (~3 sec)
#
# Real-network tests (requires TAP — see test_networking.py):
#   make test-net       All real-net tests against TAP device
#   make test-net K=X   Subset of real-net tests
#
# All targets run in the background.  Use `make test-status` or
# `make test-watch` to monitor progress.
#
# conftest.py writes live status to /tmp/megapad_test_status.json.
# test_monitor.py reads it and renders the dashboard.

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

# --- Benchmark: compare Python vs C++ ---
.PHONY: bench
bench: accel
	$(VENV_PY) bench_accel.py

# --- Primary test target: background + live dashboard (DEFAULT) ---
.PHONY: test
test: test-bg

# --- Quick smoke test: BIOS + CPU only ---
.PHONY: test-quick
test-quick: accel
	@if [ -f /tmp/megapad_test_pid.txt ] && kill -0 $$(cat /tmp/megapad_test_pid.txt) 2>/dev/null; then \
		echo "Tests already running (PID $$(cat /tmp/megapad_test_pid.txt)). Use 'make test-kill' first."; \
		exit 1; \
	fi
	@rm -f /tmp/megapad_test_status.json /tmp/megapad_test_pid.txt
	@echo "Starting quick smoke test in background..."
	@nohup env MP64_VIA_MAKE=1 $(VENV_PY) $(PYTEST_SIM) -k "TestBIOS and not test_autoboot or TestMulticore" --tb=short \
		> /tmp/megapad_test_output.txt 2>&1 & \
		echo "$$!" > /tmp/megapad_test_pid.txt
	@echo "PID: $$(cat /tmp/megapad_test_pid.txt)"
	@echo "Monitor: make test-status  |  make test-watch"

# --- Single test (usage: make test-one K=TestFoo) ---
# Runs in background with monitoring, like `make test`.
.PHONY: test-one
test-one: accel
	@if [ -z "$(K)" ]; then echo "Usage: make test-one K=TestFoo"; exit 1; fi
	@if [ -f /tmp/megapad_test_pid.txt ] && kill -0 $$(cat /tmp/megapad_test_pid.txt) 2>/dev/null; then \
		echo "Tests already running (PID $$(cat /tmp/megapad_test_pid.txt)). Use 'make test-kill' first."; \
		exit 1; \
	fi
	@rm -f /tmp/megapad_test_status.json /tmp/megapad_test_pid.txt
	@echo "Starting tests in background (K=$(K))..."
	@nohup env MP64_VIA_MAKE=1 $(VENV_PY) $(PYTEST_SIM) -n $(WORKERS) --tb=long -v -k "$(K)" \
		> /tmp/megapad_test_output.txt 2>&1 & \
		echo "$$!" > /tmp/megapad_test_pid.txt
	@echo "PID: $$(cat /tmp/megapad_test_pid.txt)"
	@echo "Monitor: make test-status  |  make test-watch"

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
	@if [ -f /tmp/megapad_test_pid.txt ] && kill -0 $$(cat /tmp/megapad_test_pid.txt) 2>/dev/null; then \
		echo "Tests already running (PID $$(cat /tmp/megapad_test_pid.txt)). Use 'make test-kill' first."; \
		exit 1; \
	fi
	@rm -f /tmp/megapad_test_status.json /tmp/megapad_test_pid.txt
	@echo "Starting real-network tests in background (TAP: $${MP64_TAP:-mp64tap0})..."
	@nohup env MP64_VIA_MAKE=1 $(VENV_PY) -m pytest test_networking.py -v --tb=long $(if $(K),-k "$(K)",) \
		> /tmp/megapad_test_output.txt 2>&1 & \
		echo "$$!" > /tmp/megapad_test_pid.txt
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
.PHONY: run disk
run:
	$(VENV_PY) cli.py --bios bios.asm --forth kdos.f

disk:
	$(VENV_PY) diskutil.py sample

