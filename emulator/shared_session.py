"""Architectural execution and diagnostics for the shared session owner."""

from __future__ import annotations

import threading
import time

from emulator.megapad64 import Megapad64Error
from emulator.session import MachineSession
from shared.session_status import SessionRuntimeDescriptor, terminal_status
from shared_session import SharedSessionOwner


class SharedMachine(SharedSessionOwner):
    """Continuously run one architectural session under the shared authority."""

    session: MachineSession

    def _phase_profile_address_valid(self, address: int) -> bool:
        """Restrict diagnostics to regions that may hold Forth variables."""

        system = self.session.system
        ram_size = int(system.ram_size)
        if 0 <= address and address + 8 <= ram_size:
            return True
        if not int(system.ext_mem_size):
            return False
        return int(system.ext_mem_base) <= address and address + 8 <= int(
            system.ext_mem_end
        )

    def _phase_profile_read(self, address: int) -> int:
        """Read the packed phase cell without changing guest state."""

        return self.session.system.cpu.mem_read64(address)

    def _phase_profile_batch_step_bound(self) -> int | None:
        """Return the most guest steps one sample interval can retire.

        None means sample intervals have no fixed size.  Every transition
        still carries the exact bounds of the interval it was seen in.
        """

        return int(self.session.batch_steps)

    def start(self):
        with self.lock:
            if self._thread is not None:
                return
            self.session.boot()
            if self._host_profile_enabled:
                self.session.system.start_host_profile()
            self._reset_generation += 1
            self._thread = threading.Thread(
                target=self._run_loop,
                name="megapad-shared-machine",
                daemon=True,
            )
            self._thread.start()

    def _run_loop(self):
        while True:
            idle_wait = False
            progress_wait = False
            with self.condition:
                if self._stopping:
                    return
                if self.paused:
                    self.condition.wait(timeout=0.1)
                    continue
                system = self.session.system
                terminal_failure = self.session.rich_terminal_failure
                if terminal_failure is not None:
                    self.last_error = f"TerminalSessionError: {terminal_failure}"
                    self.paused = True
                    continue
                terminal_pending = self.session.rich_terminal_work_pending
                if system.all_halted and not terminal_pending:
                    self.condition.wait(timeout=0.05)
                    continue
                if (
                    system.all_idle_or_halted
                    and not system.uart.has_rx_data
                    and not terminal_pending
                ):
                    idle_wait = True
                else:
                    try:
                        stats = self.session.run_batch_stats(
                            self.session.batch_steps
                        )
                        self.last_stop_reason = stats.system_stop_reason
                        executed = stats.instructions_executed
                        if executed > 0:
                            step_lower_bound = self.total_steps
                            self.total_steps += executed
                            self.total_batches += 1
                            self._sample_phase_profile(
                                step_lower_bound,
                                self.total_steps,
                                source="run_batch",
                                batch_index=self.total_batches,
                            )
                        elif not self.session.last_batch_made_progress:
                            # A bounded host queue can remain legitimately
                            # blocked until a client supplies input or another
                            # runner boundary becomes admissible.  Preserve the
                            # exact stop reason and wait instead of fake-charging
                            # a guest instruction or hot-spinning.
                            progress_wait = True
                    except Exception as exc:
                        self.last_error = f"{type(exc).__name__}: {exc}"
                        self.paused = True

            if progress_wait:
                with self.condition:
                    self.condition.wait(timeout=self.idle_sleep_s)
            elif idle_wait:
                with self.condition:
                    system = self.session.system
                    timed_wake = system.idle_wake_delay_s()
                    timeout = self.idle_wait_cap_s
                    if timed_wake is not None:
                        timeout = min(timeout, timed_wake)
                    started = time.monotonic()
                    if timeout > 0:
                        self.condition.wait(timeout=timeout)
                    if self._stopping or self.paused:
                        continue
                    system = self.session.system
                    try:
                        # Emulated time follows host time while every core
                        # sleeps, so timers and WAKE_MS deadlines come due
                        # on time, then the IDL wake rule settles.
                        system.advance_idle_time(time.monotonic() - started)
                    except Exception as exc:
                        self.last_error = f"{type(exc).__name__}: {exc}"
                        self.paused = True
            else:
                time.sleep(0)

    @staticmethod
    def _nearest_label(labels: dict[str, int], address: int) -> dict | None:
        matches = (
            (value, name) for name, value in labels.items() if value <= address
        )
        try:
            value, name = max(matches)
        except ValueError:
            return None
        return {"name": name, "address": value, "offset": address - value}

    def _forth_dictionary(self, cpu) -> tuple[list[dict], int]:
        labels = self.session.bios_labels
        latest_variable = labels.get("var_latest")
        here_variable = labels.get("var_here")
        if latest_variable is None or here_variable is None:
            return [], 0

        # Scalar CPU reads alias unmapped addresses into Bank 0, so validate
        # headers against the only two regions where Forth may build words.
        # ENTER/LEAVE-USERLAND can make the link chain alternate between them.
        system = self.session.system
        regions = [("ram", 0, int(system.ram_size))]
        if system.ext_mem_size:
            regions.append(
                ("ext", int(system.ext_mem_base), int(system.ext_mem_end))
            )

        def containing_region(address: int, count: int):
            if address < 0 or count < 0:
                return None
            end = address + count
            if end < address or end > 1 << 64:
                return None
            matches = [
                region
                for region in regions
                if region[1] <= address and end <= region[2]
            ]
            return matches[0] if len(matches) == 1 else None

        words = []
        seen = set()
        try:
            entry = int(cpu.mem_read64(latest_variable))
            here = int(cpu.mem_read64(here_variable))
            active_regions = [
                region
                for region in regions
                if region[1] <= here < region[2]
            ]
            if not active_regions:
                active_regions = [
                    region for region in regions if here == region[2]
                ]
            if len(active_regions) != 1:
                return [], 0
            ceilings = {name: limit for name, _base, limit in regions}
            ceilings[active_regions[0][0]] = here
            while entry:
                if entry in seen:
                    return words, 0
                region = containing_region(entry, 9)
                if region is None:
                    return words, 0
                region_name, _region_base, _region_limit = region
                upper = ceilings[region_name]
                if entry + 9 > upper:
                    return words, 0
                seen.add(entry)
                flags_len = int(cpu.mem_read8(entry + 8))
                name_len = flags_len & 0x7F
                code = entry + 9 + name_len
                if (
                    code > upper
                    or containing_region(entry, 9 + name_len) != region
                ):
                    return words, 0
                name = bytes(
                    int(cpu.mem_read8(entry + 9 + index))
                    for index in range(name_len)
                ).decode("ascii", errors="replace")
                word = {
                    "name": name,
                    "header": entry,
                    "code": code,
                    "_region": region_name,
                    "_upper": upper,
                }
                if code + 17 <= upper:
                    prefix = bytes(
                        int(cpu.mem_read8(code + index)) for index in range(3)
                    )
                    suffix = bytes(
                        int(cpu.mem_read8(code + 11 + index))
                        for index in range(6)
                    )
                    if (
                        prefix == b"\xf0\x60\x10"
                        and suffix == b"\x67\xe0\x08\x54\xe1\x0e"
                    ):
                        data_address = sum(
                            int(cpu.mem_read8(code + 3 + index)) << (index * 8)
                            for index in range(8)
                        )
                        if containing_region(data_address, 8) is not None:
                            word["data_address"] = data_address
                            word["value"] = int(cpu.mem_read64(data_address))
                words.append(word)
                ceilings[region_name] = entry
                next_entry = int(cpu.mem_read64(entry))
                entry = next_entry
        except (IndexError, Megapad64Error, RuntimeError, ValueError):
            return words, 0

        # The physical end safely bounds an inactive region during traversal,
        # but it is too broad for instruction-address lookup.  KDOS records
        # each inactive dictionary's exact saved HERE before switching banks.
        saved_here_words = {
            "ram": "SYS-HERE-SAVE",
            "ext": "U-DICT-HERE",
        }
        active_region = active_regions[0][0]
        for region_name, base, limit in regions:
            if region_name == active_region:
                continue
            newest = next(
                (word for word in words if word["_region"] == region_name),
                None,
            )
            saved_candidates = [
                word
                for word in words
                # KDOS owns the oldest Bank-0 definition; later shadows are
                # ordinary Forth words, not dictionary-switch state.
                if word["_region"] == "ram"
                and word["name"].upper() == saved_here_words[region_name]
                and "value" in word
            ]
            saved = int(saved_candidates[-1]["value"]) if saved_candidates else 0
            if newest is None or saved == 0:
                continue
            if not (base <= saved <= limit and newest["code"] <= saved):
                return words, 0
            newest["_upper"] = saved
        return words, here

    @staticmethod
    def _forth_word_at(words: list[dict], here: int, address: int) -> dict | None:
        for word in words:
            code = word["code"]
            upper = word.get("_upper", here)
            if code <= address < upper:
                return {
                    "name": word["name"],
                    "header": word["header"],
                    "code": code,
                    "offset": address - code,
                }
        return None

    def _forth_diagnostics(self, cpu) -> dict:
        registers = [int(value) for value in cpu.regs]

        def cells(address: int, count: int = 8) -> list[int]:
            values = []
            for index in range(count):
                try:
                    values.append(int(cpu.mem_read64(address + index * 8)))
                except (IndexError, RuntimeError, ValueError):
                    break
            return values

        ip = registers[3]
        labels = self.session.bios_labels
        words, here = self._forth_dictionary(cpu)
        return_stack = cells(registers[15])
        result = {
            "instruction_pointer": ip,
            "data_stack_pointer": registers[14],
            "return_stack_pointer": registers[15],
            "data_stack": cells(registers[14]),
            "return_stack": return_stack,
            "return_words": [
                self._forth_word_at(words, here, address)
                or self._nearest_label(labels, address)
                for address in return_stack
            ],
            "bios_primitive": self._nearest_label(labels, int(cpu.pc)),
            "word": self._forth_word_at(words, here, ip),
        }
        return result

    def forth(self, names: list[str]) -> dict:
        with self.lock:
            words, here = self._forth_dictionary(self.session.system.cpu)
            wanted = {str(name).upper() for name in names}
            found = {}
            for word in words:
                key = word["name"].upper()
                if key in wanted and key not in found:
                    found[key] = {
                        field: value
                        for field, value in word.items()
                        if not field.startswith("_")
                    }
            return {"here": here, "words": found}

    def peek(self, address: int, count: int = 1) -> dict:
        address = int(address)
        count = int(count)
        if address < 0 or not (1 <= count <= 256):
            raise ValueError("peek requires a non-negative address and 1..256 cells")
        with self.lock:
            cpu = self.session.system.cpu
            return {
                "address": address,
                "cell_size": 8,
                "values": [
                    int(cpu.mem_read64(address + index * 8))
                    for index in range(count)
                ],
            }

    def status(self, *, detailed: bool = True) -> dict:
        """Return machine status.

        Detailed status remains the default for control and diagnostic
        clients.  High-frequency observers such as the session viewer can
        opt out of CPU/Forth/network diagnostics, most notably avoiding a
        complete Forth dictionary walk while holding the machine lock.
        """
        with self.lock:
            system = self.session.system
            cpu = system.cpu
            rich_terminal_failure = self.session.rich_terminal_failure
            rich_terminal_pending = self.session.rich_terminal_work_pending
            quiescent = not system.uart.has_rx_data and not rich_terminal_pending
            operational = rich_terminal_failure is None
            halted = system.all_halted
            idle = system.all_idle_or_halted and quiescent and operational
            visible_cols, visible_rows = self.session.visible_geometry
            if self.session.rich_terminal_lost:
                state = "lost"
            elif rich_terminal_failure is not None:
                state = "terminal_failed"
            elif self.last_error:
                state = "error"
            elif self.paused:
                state = "paused"
            elif halted and not rich_terminal_pending and operational:
                state = "halted"
            elif idle:
                state = "idle"
            elif self.last_stop_reason == "host_backpressure":
                state = "backpressured"
            else:
                state = "running"
            result = {
                "runtime": SessionRuntimeDescriptor(
                    mode="emulator",
                    executor="native",
                    step_unit="mp64_instruction",
                    step_request_unit="mp64_instruction",
                    batch_unit="instruction_batch",
                    timer_unit="mp64_system_cycle",
                    rtc_mode=system.rtc.clock_mode,
                    machine_code=True,
                    cpu_diagnostics=True,
                    network_diagnostics=True,
                    reset=True,
                    host_profiling=True,
                ).to_dict(),
                "generation": self._reset_generation,
                "state": state,
                "paused": self.paused,
                "halted": halted,
                "idle": idle,
                "stop_reason": self.last_stop_reason,
                "steps": self.total_steps,
                "batches": self.total_batches,
                "revision": self.session.revision,
                "raw_bytes": self.session.raw_output_end,
                "raw_start": self.session.raw_output_start,
                "raw_offset": self.session.raw_output_end,
                "raw_retained_bytes": len(self.session.raw_output),
                "output_batches": self.session.output_batches,
                "byte_callbacks": self.session.output_byte_callbacks,
                "terminal": [visible_cols, visible_rows],
                "uptime_s": time.time() - self.started_at,
                "error": self.last_error,
                "rich_terminal": terminal_status(
                    self.session,
                    pending=rich_terminal_pending,
                    failure=rich_terminal_failure,
                ),
            }
            if not detailed:
                return result

            backend = system.nic.backend
            result.update(
                {
                    "cpu": {
                        "pc": cpu.pc,
                        "cycles": cpu.cycle_count,
                        "registers": [int(value) for value in cpu.regs],
                        "psel": cpu.psel,
                        "xsel": cpu.xsel,
                        "spsel": cpu.spsel,
                    },
                    "forth": self._forth_diagnostics(cpu),
                    "clock": {
                        "mode": system.rtc.clock_mode,
                        "uptime_ms": system.rtc.uptime_ms,
                        "epoch_ms": system.rtc.epoch_ms,
                    },
                    "nic": {
                        "backend": system.nic.backend_name,
                        "link_up": system.nic.link_up,
                        "tx_frames": getattr(
                            backend, "tx_frames", system.nic.tx_count
                        ),
                        "rx_frames": getattr(backend, "rx_frames", 0),
                        "rx_queued": cpu._cs.nic_rx_queue_size(),
                    },
                }
            )
            if self._host_profile_enabled:
                result["host_profile"] = system.host_profile_snapshot()
            return result

    def network(self) -> dict:
        with self.lock:
            system = self.session.system
            backend = system.nic.backend
            result = {
                "backend": system.nic.backend_name,
                "link_up": system.nic.link_up,
                "guest_tx_frames": system.nic.tx_count,
                "guest_rx_frames": system.cpu._cs.nic_get_rx_count(),
                "guest_rx_queued": system.cpu._cs.nic_rx_queue_size(),
            }
            if backend is not None and hasattr(backend, "stats"):
                result["transport"] = backend.stats()
            return result

    def step(self, count: int = 1) -> dict:
        count = int(count)
        if count <= 0 or count > 1_000_000:
            raise ValueError("step count must be between 1 and 1000000")
        with self.condition:
            if not self.paused:
                raise RuntimeError("machine must be paused before stepping")
            terminal_failure = self.session.rich_terminal_failure
            if terminal_failure is not None or self.session.rich_terminal_lost:
                self.last_error = (
                    "TerminalSessionError: "
                    f"{terminal_failure or 'rich-terminal attachment lost'}"
                )
                raise RuntimeError(
                    "rich terminal failure requires a machine reset: "
                    f"{terminal_failure or 'attachment lost'}"
                )
            executed = 0
            cycles = 0
            stop_reason = "instruction_limit"
            for _ in range(count):
                if (
                    self.session.system.all_halted
                    and not self.session.rich_terminal_work_pending
                ):
                    stop_reason = "all_halted"
                    break
                try:
                    stats = self.session.run_batch_stats(1)
                except Exception as exc:
                    self.last_error = f"{type(exc).__name__}: {exc}"
                    self.paused = True
                    raise
                stop_reason = stats.system_stop_reason
                cycles += stats.system_cycles_advanced
                batch_executed = stats.instructions_executed
                executed += batch_executed
                if batch_executed == 0:
                    break
                step_lower_bound = self.total_steps
                self.total_steps += batch_executed
                self._sample_phase_profile(
                    step_lower_bound,
                    self.total_steps,
                    source="step",
                    batch_index=None,
                )
            self.last_stop_reason = stop_reason
            return {
                "executed": executed,
                "cycles": cycles,
                "stop_reason": stop_reason,
                "status": self.status(),
            }

    def reset(self, *, paused: bool | None = None) -> dict:
        with self.condition:
            if paused is not None and not isinstance(paused, bool):
                raise TypeError("reset paused must be a boolean or null")
            self._phase_profile = None
            try:
                self.session.reset()
                if self._host_profile_enabled:
                    self.session.system.start_host_profile()
            except Exception as exc:
                self.last_error = f"{type(exc).__name__}: {exc}"
                self.paused = True
                self.condition.notify_all()
                raise
            self.total_steps = 0
            self.total_batches = 0
            self.last_error = None
            self.last_stop_reason = "reset"
            self._reset_generation += 1
            if paused is not None:
                self.paused = paused
            self.condition.notify_all()
            return self.status()
