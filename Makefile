# Megapad-64 — Build & Test
# ==========================
#
# The C++ accelerator (~50x faster than CPython, ~10x faster than PyPy)
# is the DEFAULT test backend.  All targets auto-build it.
#
#   make test                  Sequential background + dashboard (DEFAULT)
#   make test K=X              Sequential background subset
#   make test-one K=X          Sequential single test/class + monitoring
#   make test-sequential       Sequential foreground suite
#   make test-sequential K=X   Sequential foreground subset
#   make test-status           One-shot progress dashboard
#   make test-watch            Auto-refresh dashboard every 5s
#   make test-failures         Show only failures
#   make test-kill             Kill stuck background run
#   make test-quick            Quick BIOS+CPU smoke test     (~3 sec)
#
# Real-network tests (requires TAP — see tests/test_live_net.py):
#   make test-net       All live-net tests against TAP device
#   make test-net K=X   Subset of live-net tests
#
# All background targets use `make test-status` / `make test-watch`
# to monitor progress.
#
# tests/conftest.py writes live status to /tmp/megapad_test_status.json.
# test_monitor.py reads it and renders the dashboard.  Set
# MP64_RUNTIME_NAMESPACE to isolate these files for a parallel checkout.

.DEFAULT_GOAL := test

VENV_PY  ?= $(if $(wildcard .venv/bin/python),.venv/bin/python,python3)
PYTEST   := -m pytest tests/
PYTEST_CONFIG_ARGS := -o addopts=
PYTEST_ARGS := $(PYTEST_CONFIG_ARGS) --tb=long
TEST_PATH ?= tests/

export MP64_RUNTIME_NAMESPACE
RUNTIME_PATHS := python3 runtime_paths.py
TEST_SUPERVISOR := python3 test_process_supervisor.py

define RESOLVE_TEST_PATHS
status_file="$$( $(RUNTIME_PATHS) test-status)" || exit $$?; \
pid_file="$$( $(RUNTIME_PATHS) test-pid)" || exit $$?; \
output_file="$$( $(RUNTIME_PATHS) test-output)" || exit $$?;
endef

.PHONY: runtime-paths
runtime-paths:
	@$(RUNTIME_PATHS) all

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

# --- Foreground sequential test run ---
# Usage: make test-sequential
#        make test-sequential TEST_PATH=tests/test_system.py
#        make test-sequential K=TestFoo
.PHONY: test-sequential
test-sequential: accel
	@set -eu; \
	$(RESOLVE_TEST_PATHS) \
	exec $(TEST_SUPERVISOR) foreground \
		--state "$$pid_file" --status "$$status_file" -- \
		env MP64_VIA_MAKE=1 PYTEST_ADDOPTS= \
			$(VENV_PY) -m pytest $(TEST_PATH) \
			$(PYTEST_CONFIG_ARGS) --tb=long $(if $(K),-k "$(K)",)

# --- Quick smoke test: BIOS + CPU only ---
.PHONY: test-quick
test-quick: accel
	@set -eu; \
	$(RESOLVE_TEST_PATHS) \
	echo "Starting quick smoke test in background..."; \
	$(TEST_SUPERVISOR) start \
		--state "$$pid_file" --status "$$status_file" --output "$$output_file" -- \
		env MP64_VIA_MAKE=1 PYTEST_ADDOPTS= $(VENV_PY) $(PYTEST) $(PYTEST_CONFIG_ARGS) -k "TestBIOS and not test_autoboot or TestMulticore" --tb=short; \
	echo "Monitor: make test-status  |  make test-watch"

# --- Single sequential test (usage: make test-one K=TestFoo) ---
.PHONY: test-one
test-one: accel
	@set -eu; \
	$(RESOLVE_TEST_PATHS) \
	if [ -z "$(K)" ]; then echo "Usage: make test-one K=TestFoo"; exit 1; fi; \
	echo "Starting tests in background (K=$(K))..."; \
	$(TEST_SUPERVISOR) start \
		--state "$$pid_file" --status "$$status_file" --output "$$output_file" -- \
		env MP64_VIA_MAKE=1 PYTEST_ADDOPTS= $(VENV_PY) $(PYTEST) $(PYTEST_CONFIG_ARGS) --tb=long -v -k "$(K)"; \
	echo "Monitor: make test-status  |  make test-watch"

# --- Sequential background test run with live monitoring ---
# Usage: make test-bg          (full suite)
#        make test-bg K=TestFoo (subset)
# Then:  make test-status  or  make test-watch
.PHONY: test-bg
test-bg: accel
	@set -eu; \
	$(RESOLVE_TEST_PATHS) \
	echo "Starting tests in background (C++ accel)..."; \
	if [ -n "$(K)" ]; then \
		$(TEST_SUPERVISOR) start \
			--state "$$pid_file" --status "$$status_file" --output "$$output_file" -- \
			env MP64_VIA_MAKE=1 PYTEST_ADDOPTS= $(VENV_PY) $(PYTEST) $(PYTEST_ARGS) -k "$(K)"; \
	else \
		$(TEST_SUPERVISOR) start \
			--state "$$pid_file" --status "$$status_file" --output "$$output_file" -- \
			env MP64_VIA_MAKE=1 PYTEST_ADDOPTS= $(VENV_PY) $(PYTEST) $(PYTEST_ARGS); \
	fi; \
	echo "Monitor: make test-status  |  make test-watch"

# --- Real-network tests (requires TAP device) ---
# Usage: make test-net              (all live-net tests)
#        make test-net K=TestLiveARP (subset)
.PHONY: test-net
test-net: accel
	@set -eu; \
	$(RESOLVE_TEST_PATHS) \
	echo "Starting live-network tests in background (TAP: $${MP64_TAP:-mp64tap0})..."; \
	$(TEST_SUPERVISOR) start \
		--state "$$pid_file" --status "$$status_file" --output "$$output_file" -- \
		env MP64_VIA_MAKE=1 PYTEST_ADDOPTS= $(VENV_PY) -m pytest tests/test_live_net.py tests/test_networking.py $(PYTEST_CONFIG_ARGS) -v --tb=long $(if $(K),-k "$(K)",); \
	echo "Monitor: make test-status  |  make test-watch"

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
	@set -eu; \
	pid_file="$$( $(RUNTIME_PATHS) test-pid)" || exit $$?; \
	$(TEST_SUPERVISOR) stop --state "$$pid_file"

# --- Run the interactive emulator ---
.PHONY: run disk
run:
	$(VENV_PY) cli.py --bios bios.asm --forth kdos.f

disk:
	$(VENV_PY) diskutil.py sample
