"""Semantic BIOS compatibility words used by KDOS application loading."""

from __future__ import annotations

from shared.cells import MASK64
from simulator.runtime import MegaForthRuntime


APPLICATION_BIOS_WORDS = (
    "ENTER-USER",
    "SYS-EXIT",
    "PRIV@",
    "MPU-BASE!",
    "MPU-LIMIT!",
    "MPU-BASE@",
    "MPU-LIMIT@",
)


def _execute(
    runtime: MegaForthRuntime,
    name: str,
    *values: int,
) -> tuple[int, ...]:
    context = runtime.main_context
    context.data.clear()
    for value in values:
        context.data.push(value)
    runtime.execute(name)
    return context.data.snapshot()


def test_retired_user_mode_words_are_stack_neutral_and_always_supervisor() -> None:
    runtime = MegaForthRuntime()

    assert all(runtime.find(name) is not None for name in APPLICATION_BIOS_WORDS)
    assert runtime.privilege_level == 0
    assert _execute(runtime, "ENTER-USER", 0xA5) == (0xA5,)
    assert _execute(runtime, "PRIV@") == (0,)
    assert _execute(runtime, "SYS-EXIT", 0x5A) == (0x5A,)
    assert runtime.privilege_level == 0


def test_mpu_registers_retain_cells_without_enforcing_memory_access() -> None:
    runtime = MegaForthRuntime()
    outside = runtime.define_created("OUTSIDE-MPU", initial_body=bytes(8))

    assert runtime.mpu_base == 0
    assert runtime.mpu_limit == 0
    assert _execute(runtime, "MPU-BASE@") == (0,)
    assert _execute(runtime, "MPU-LIMIT@") == (0,)

    assert _execute(runtime, "MPU-BASE!", MASK64) == ()
    assert _execute(runtime, "MPU-LIMIT!", 1) == ()
    assert runtime.mpu_base == MASK64
    assert runtime.mpu_limit == 1
    assert _execute(runtime, "MPU-BASE@") == (MASK64,)
    assert _execute(runtime, "MPU-LIMIT@") == (1,)

    # Native access checks are gated by the retired user privilege bit.  The
    # hosted registers therefore retain values but cannot reject this ordinary
    # guest memory operation outside the nonsensical window above.
    assert _execute(
        runtime,
        "!",
        0x1122_3344_5566_7788,
        outside.body_address,
    ) == ()
    assert _execute(runtime, "@", outside.body_address) == (
        0x1122_3344_5566_7788,
    )


def test_mpu_state_is_runtime_local_and_user_transitions_do_not_change_it() -> None:
    first = MegaForthRuntime()
    second = MegaForthRuntime()

    _execute(first, "MPU-BASE!", 0x1000)
    _execute(first, "MPU-LIMIT!", 0x2000)
    _execute(first, "ENTER-USER")
    _execute(first, "SYS-EXIT")

    assert (first.mpu_base, first.mpu_limit) == (0x1000, 0x2000)
    assert (second.mpu_base, second.mpu_limit) == (0, 0)
