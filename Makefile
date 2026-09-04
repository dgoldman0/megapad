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
#   make test-sanitize         Foreground Phase 3 ASan+UBSan subset
#   make test-sanitize SANITIZER=thread
#   make test-rich-terminal-simulator  Fast shared-source simulator oracle
#   make test-rich-terminal-emulator   Exact-machine shared-source oracle
#   make test-rich-terminal-dual       Both shared-source backends
#   make test-status           One-shot progress dashboard
#   make test-watch            Auto-refresh dashboard every 5s
#   make test-failures         Show only failures
#   make test-kill             Kill stuck background run
#   make test-quick            Quick BIOS+CPU smoke test     (~3 sec)
#   make test-simulator        Focused hosted-simulator units (no accel build)
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
SIMULATOR_TEST_PATH ?= tests/simulator/
SANITIZER ?= address-undefined
SANITIZE_BUILD_ROOT ?= $(CURDIR)/build/sanitizers
SANITIZE_TEST_PATHS ?= \
	tests/test_phase3_worker_pool.py \
	tests/test_phase3_private_execution.py \
	tests/test_phase3_coordinator_execution.py \
	tests/test_phase3_reduced_core_execution.py \
	tests/test_phase3_event_execution.py \
	tests/test_phase3_benchmark.py

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

# --- Hosted source simulator ---
# This target intentionally does not depend on accel.  Simulator units are
# seconds-scale semantic checks and must not compile or execute the exact MP64
# backend merely to prove backend-independent behavior.
.PHONY: test-simulator
test-simulator:
	@set -eu; \
	$(RESOLVE_TEST_PATHS) \
	exec $(TEST_SUPERVISOR) foreground \
		--state "$$pid_file" --status "$$status_file" -- \
		env MP64_VIA_MAKE=1 PYTEST_ADDOPTS= \
			$(VENV_PY) -m pytest $(SIMULATOR_TEST_PATH) \
			$(PYTEST_CONFIG_ARGS) --tb=long $(if $(K),-k "$(K)",)

# --- Shared rich-terminal production-source oracles ---
# The simulator selector stays accelerator-free for the tight development
# loop. The emulator and combined selectors use the ordinary sequential
# exact-machine target as the acceptance backstop.
.PHONY: test-rich-terminal-simulator
test-rich-terminal-simulator:
	@$(MAKE) test-simulator \
		SIMULATOR_TEST_PATH=tests/test_rich_terminal_dual_backend.py \
		K=simulator

.PHONY: test-rich-terminal-emulator
test-rich-terminal-emulator:
	@$(MAKE) test-sequential \
		TEST_PATH=tests/test_rich_terminal_dual_backend.py \
		K=emulator

.PHONY: test-rich-terminal-dual
test-rich-terminal-dual:
	@$(MAKE) test-sequential \
		TEST_PATH=tests/test_rich_terminal_dual_backend.py

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

# --- Isolated sanitizer build + bounded foreground Phase 3 suite ---
# The public target enters the same foreground supervisor used by ordinary
# tests before building, so sanitizer configurations cannot overlap each
# other or an ordinary supervised test run.
.PHONY: test-sanitize _test-sanitize-run
test-sanitize:
	@set -eu; \
	case "$(SANITIZER)" in \
		address-undefined|thread) ;; \
		*) \
			echo "SANITIZER must be address-undefined or thread" >&2; \
			exit 2; \
			;; \
	esac; \
	$(RESOLVE_TEST_PATHS) \
	exec $(TEST_SUPERVISOR) foreground \
		--state "$$pid_file" --status "$$status_file" -- \
		$(MAKE) --no-print-directory _test-sanitize-run \
			SANITIZER="$(SANITIZER)" \
			SANITIZE_BUILD_ROOT="$(SANITIZE_BUILD_ROOT)" \
			SANITIZE_TEST_PATHS="$(SANITIZE_TEST_PATHS)" \
			K="$(K)"

# Private half of test-sanitize. Keep the build out of --inplace so neither
# the optimized extension nor the existing reports in build/ are disturbed.
_test-sanitize-run:
	@set -eu; \
	sanitizer_root="$(SANITIZE_BUILD_ROOT)/$(SANITIZER)"; \
	sanitizer_temp="$$sanitizer_root/temp"; \
	sanitizer_lib="$$sanitizer_root/lib"; \
	MP64_ACCEL_SANITIZER="$(SANITIZER)" \
		$(VENV_PY) setup_accel.py build_ext --force \
			--build-temp "$$sanitizer_temp" \
			--build-lib "$$sanitizer_lib"; \
	sanitizer_runtime=""; \
	cxx_runtime=""; \
	sanitizer_launcher=""; \
	sanitizer_preload=""; \
	case "$(SANITIZER)" in \
		address-undefined) \
			sanitizer_runtime="$$( $(CXX) -print-file-name=libasan.so )"; \
			cxx_runtime="$$( $(CXX) -print-file-name=libstdc++.so )"; \
			ASAN_OPTIONS="$${ASAN_OPTIONS:+$${ASAN_OPTIONS}:}detect_leaks=0:halt_on_error=1:abort_on_error=1"; \
			UBSAN_OPTIONS="$${UBSAN_OPTIONS:+$${UBSAN_OPTIONS}:}halt_on_error=1:abort_on_error=1:print_stacktrace=1"; \
			export ASAN_OPTIONS UBSAN_OPTIONS; \
			;; \
		thread) \
			sanitizer_runtime="$$( $(CXX) -print-file-name=libtsan.so )"; \
			sanitizer_arch="$$(uname -m)"; \
			if ! setarch "$$sanitizer_arch" -R true >/dev/null 2>&1; then \
				echo "ThreadSanitizer requires setarch -R on this host" >&2; \
				exit 2; \
			fi; \
			sanitizer_launcher="setarch $$sanitizer_arch -R"; \
			TSAN_OPTIONS="$${TSAN_OPTIONS:+$${TSAN_OPTIONS}:}halt_on_error=1:abort_on_error=1"; \
			export TSAN_OPTIONS; \
			;; \
		*) \
			echo "SANITIZER must be address-undefined or thread" >&2; \
			exit 2; \
			;; \
	esac; \
	if [ -n "$$sanitizer_runtime" ]; then \
		if [ ! -f "$$sanitizer_runtime" ]; then \
			echo "Sanitizer runtime is unavailable: $$sanitizer_runtime" >&2; \
			exit 2; \
		fi; \
		preload_runtime="$$sanitizer_runtime"; \
		if [ -n "$$cxx_runtime" ]; then \
			if [ ! -f "$$cxx_runtime" ]; then \
				echo "C++ runtime is unavailable: $$cxx_runtime" >&2; \
				exit 2; \
			fi; \
			preload_runtime="$$preload_runtime:$$cxx_runtime"; \
		fi; \
		sanitizer_preload="$$preload_runtime$${LD_PRELOAD:+:$${LD_PRELOAD}}"; \
	fi; \
	PYTHONSAFEPATH=1; \
	PYTHONPATH="$$sanitizer_lib:$(CURDIR)$${PYTHONPATH:+:$${PYTHONPATH}}"; \
	MP64_ACCEL_SANITIZER="$(SANITIZER)"; \
	export PYTHONSAFEPATH PYTHONPATH MP64_ACCEL_SANITIZER; \
	$$sanitizer_launcher env LD_PRELOAD="$$sanitizer_preload" \
		$(VENV_PY) -P -c \
		'import pathlib, sys, _mp64_accel; root = pathlib.Path(sys.argv[1]).resolve(); loaded = pathlib.Path(_mp64_accel.__file__).resolve(); print("sanitizer module:", loaded); raise SystemExit(0 if root in loaded.parents else "refusing non-isolated accelerator: " + str(loaded))' \
		"$$sanitizer_lib"; \
	exec $$sanitizer_launcher env \
		LD_PRELOAD="$$sanitizer_preload" \
		MP64_VIA_MAKE=1 PYTEST_ADDOPTS= \
		$(VENV_PY) -P -m pytest $(SANITIZE_TEST_PATHS) \
			$(PYTEST_CONFIG_ARGS) --import-mode=importlib \
			-p no:xdist -p no:xdist.looponfail \
			--tb=long --maxfail=1 $(if $(K),-k "$(K)",)

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
