"""Bounded integer-routine ABI against the ordinary MP64 interpreter.

These are architectural runner checks. Registration sealing, allocation
leases, and semantic stack settlement belong to the composition tests.
"""

from __future__ import annotations

import gc

import _mp64_accel as native
import pytest

from asm import assemble


MASK64 = (1 << 64) - 1
CODE_BASE = 0x100
RAM_SIZE = 4096
EXT_BASE = 0x100000
EXT_SIZE = 256
CONTROL_BASE = EXT_BASE + EXT_SIZE
CONTROL_SIZE = 128
MMIO_BASE = 0xFFFFFF0000000000
MMIO_LIMIT = 0xFFFFFF8000000000


def _image(program, *, base=CODE_BASE):
    raw = bytes(assemble(program, base_addr=base)) if isinstance(program, str) else program
    return raw + b"\x01" * (-len(raw) % 16)


class _Harness:
    def __init__(self, program="ret.l", *, inputs=0, outputs=0,
                 max_instructions=1000, stack_size=CONTROL_SIZE, ext_size=EXT_SIZE,
                 extra_regions=()):
        self.ram = bytearray(RAM_SIZE)
        self.external = bytearray(ext_size)
        self.control = bytearray(CONTROL_SIZE)
        self.image = _image(program)
        self.ram[CODE_BASE:CODE_BASE + len(self.image)] = self.image
        self.state = native.CPUState()
        self.state.attach_mem(self.ram, len(self.ram))
        if ext_size:
            self.state.attach_ext_mem(self.external, EXT_BASE, ext_size)
        for method, base, buffer in extra_regions:
            getattr(self.state, method)(buffer, base, len(buffer))
        self.state.icache_control_write(1)
        self.runner = native.RoutineRunnerV1(self.state, CONTROL_BASE, self.control)
        self.spec = native.RoutineSpecV1(
            CODE_BASE, len(self.image), 0, inputs, outputs,
            CONTROL_BASE, stack_size, max_instructions,
        )
        self.runner.publish_code(self.spec)

    def run(self, arguments=(), spans=(), *, limit=1000, **kwargs):
        return self.runner.run(self.spec, arguments, spans, limit, **kwargs)


def _ordinary_reference(harness, arguments, *, limit=1000):
    ram = bytearray(harness.ram)
    external = bytearray(EXT_SIZE + CONTROL_SIZE)
    external[:len(harness.external)] = harness.external
    state = native.CPUState()
    state.attach_mem(ram, len(ram))
    state.attach_ext_mem(external, EXT_BASE, len(external))
    state.icache_control_write(1)
    state.psel, state.xsel, state.spsel = 3, 2, 15
    state.set_reg(3, harness.spec.code_base + harness.spec.entry_offset)
    stack_top = harness.spec.stack_base + harness.spec.stack_size
    state.set_reg(15, stack_top - 8)
    root_offset = stack_top - 8 - EXT_BASE
    external[root_offset:root_offset + 8] = MASK64.to_bytes(8, "little")
    for index, value in enumerate(arguments):
        state.set_reg(4 + index, value)

    def unexpected(*_args):
        pytest.fail("ordinary integer fixture reached a device callback")

    instructions = cycles = 0
    while state.get_reg(3) != MASK64:
        assert instructions < limit, "ordinary reference failed to return"
        cycles += native.step_one(
            state, mmio_read8=unexpected, mmio_write8=unexpected,
            on_output=unexpected, csr_read_override=None,
            mmio_start=MMIO_BASE, mmio_end=MMIO_LIMIT,
        )
        instructions += 1
    return state, ram, external, instructions, cycles


BUFFER_ROUTINE = """
    mov r6, r4
    ldi r4, 0
again:
    ldn r7, r6
    add r4, r7
    inc r7
    str r6, r7
    addi r6, 8
    subi r5, 1
    brne again
    ret.l
"""

NESTED_ROUTINE = """
    mov r6, r4
    ldi64 r8, helper
    call.l r8
    addi r4, 3
    ret.l
helper:
    ldi64 r9, inner
    call.l r9
    ret.l
inner:
    ldn r4, r6
    addi r4, 7
    str r6, r4
    ret.l
"""


@pytest.mark.parametrize("program,arguments,initial", [
    (BUFFER_ROUTINE, (EXT_BASE, 3), (3, 5, 7)),
    (NESTED_ROUTINE, (EXT_BASE,), (3,)),
])
def test_admitted_routines_match_ordinary_registers_memory_and_accounting(
    program, arguments, initial,
):
    harness = _Harness(program, inputs=len(arguments), outputs=1)
    payload = b"".join(value.to_bytes(8, "little") for value in initial)
    harness.external[:len(payload)] = payload
    reference, ram, external, instructions, cycles = _ordinary_reference(harness, arguments)

    result = harness.run(arguments, [(EXT_BASE, len(payload), "read_write")])

    assert result.exit_kind == "returned"
    assert result.instructions == instructions
    assert result.cycles == cycles
    assert result.outputs == (reference.get_reg(4),)
    assert [harness.state.get_reg(index) for index in range(32)] == [
        reference.get_reg(index) for index in range(32)
    ]
    assert harness.state.flags_pack() == reference.flags_pack()
    assert bytes(harness.ram) == bytes(ram)
    assert bytes(harness.external) == bytes(external[:EXT_SIZE])
    assert bytes(harness.control) == bytes(external[EXT_SIZE:])
    assert (harness.state.psel, harness.state.xsel, harness.state.spsel) == (3, 2, 15)


@pytest.mark.parametrize("inputs", [(), tuple(range(8))])
def test_zero_and_eight_cell_signatures_return_fixed_registers(inputs):
    harness = _Harness(inputs=len(inputs), outputs=len(inputs))
    for index in range(32):
        harness.state.set_reg(index, MASK64)
    harness.state.flag_i = 1

    result = harness.run(inputs, limit=1)

    assert result.exit_kind == "returned"
    assert result.outputs == inputs
    assert result.instructions == 1
    assert harness.state.get_reg(15) == CONTROL_BASE + CONTROL_SIZE
    assert harness.state.get_reg(14) == 0
    assert harness.state.flag_i == 0
    assert result.pc == native.HYBRID_ROOT_RETURN_V1 == MASK64


def test_return_on_last_instruction_succeeds_and_smaller_budget_preserves_prefix():
    harness = _Harness("inc r4\nret.l", inputs=1, outputs=1, max_instructions=2)
    short = harness.run((10,), limit=1)
    assert short.exit_kind == "instruction_limit"
    assert short.instructions == 1
    assert short.outputs == ()
    assert harness.state.get_reg(4) == 11

    exact = harness.run((10,), limit=2)
    assert exact.exit_kind == "returned"
    assert exact.instructions == 2
    assert exact.outputs == (11,)


def test_loop_obeys_the_smaller_declaration_or_call_allowance():
    harness = _Harness("again:\n br again", max_instructions=7)
    for allowance, expected in ((3, 3), (10, 7)):
        result = harness.run(limit=allowance)
        assert result.exit_kind == "instruction_limit"
        assert result.instructions == expected
        assert result.outputs == ()
        assert result.pc == CODE_BASE


def test_branch_to_root_sentinel_is_not_a_return():
    harness = _Harness("mov r3, r4", inputs=1)
    result = harness.run((MASK64,))
    assert result.exit_kind == "invalid_return"
    assert result.instructions == 1
    assert result.outputs == ()
    assert harness.state.get_reg(15) == CONTROL_BASE + CONTROL_SIZE - 8


def test_nested_call_stack_overflow_retains_completed_push_and_failed_decrement():
    harness = _Harness("call.l r4\nret.l", inputs=1, stack_size=16)
    result = harness.run((CODE_BASE,))
    assert result.exit_kind == "rejected_access"
    assert result.instructions == 1
    assert result.access_address == CONTROL_BASE - 8
    assert result.access_width == 8
    assert harness.state.get_reg(15) == CONTROL_BASE - 8
    assert int.from_bytes(harness.control[:8], "little") == CODE_BASE + 2
    assert int.from_bytes(harness.control[8:16], "little") == MASK64


@pytest.mark.parametrize("address,width", [
    (EXT_BASE + EXT_SIZE - 1, 8), (MASK64 - 3, 8),
    (RAM_SIZE + 0x800, 1), (MMIO_BASE + 0x780, 1),
    (CONTROL_BASE, 1), (CODE_BASE, 1),
])
def test_computed_accesses_cannot_escape_borrowed_spans_or_alias_bank0(address, width):
    operation = "str r4, r5" if width == 8 else "st.b r4, r5"
    harness = _Harness(operation + "\nret.l", inputs=2)
    harness.state.init_crypto()
    before = bytes(harness.ram), bytes(harness.external)
    crypto_status = harness.state.crypto_read8(0x781)

    result = harness.run((address, 1), [(EXT_BASE, EXT_SIZE, "read_write")])

    assert result.exit_kind == "rejected_access"
    assert result.instructions == 0
    assert result.access_address == address
    assert result.access_width == width
    assert result.access_operation
    assert (bytes(harness.ram), bytes(harness.external)) == before
    assert harness.state.crypto_read8(0x781) == crypto_status


def test_complete_scalar_access_must_fit_one_permission_span():
    harness = _Harness("str r4, r5\nret.l", inputs=2)
    harness.external[:16] = b"original-payload"
    before = bytes(harness.external)
    result = harness.run((EXT_BASE + 4, MASK64), [
        (EXT_BASE, 8, "write"), (EXT_BASE + 8, 8, "write"),
    ])
    assert result.exit_kind == "rejected_access"
    assert result.instructions == 0
    assert bytes(harness.external) == before


@pytest.mark.parametrize("operation,permission", [
    ("str r4, r5", "read"), ("ldn r5, r4", "write"),
])
def test_borrowed_permissions_are_enforced(operation, permission):
    harness = _Harness(operation + "\nret.l", inputs=2)
    result = harness.run((EXT_BASE, 17), [(EXT_BASE, 8, permission)])
    assert result.exit_kind == "rejected_access"
    assert result.instructions == 0
    assert bytes(harness.external) == bytes(EXT_SIZE)


@pytest.mark.parametrize("method,base", [
    ("attach_hbw_mem", 0xFFD00000), ("attach_vram", 0xFF000000),
])
def test_optional_region_end_is_shared_and_boundary_checked(method, base):
    backing = bytearray(32)
    harness = _Harness("str r4, r5\nldn r4, r4\nret.l", inputs=2, outputs=1,
                       extra_regions=[(method, base, backing)])
    result = harness.run((base + 24, MASK64), [(base, 32, "read_write")])
    assert result.exit_kind == "returned"
    assert result.outputs == (MASK64,)
    assert bytes(backing) == bytes(24) + b"\xff" * 8

    before = bytes(backing)
    rejected = harness.run((base + 25, 0), [(base, 32, "read_write")])
    assert rejected.exit_kind == "rejected_access"
    assert rejected.instructions == 0
    assert bytes(backing) == before


def test_absent_optional_region_cannot_be_borrowed():
    harness = _Harness(ext_size=0)
    with pytest.raises(ValueError):
        harness.run(spans=[(EXT_BASE, 8, "read")])
    assert bytes(harness.control) == bytes(CONTROL_SIZE)


def test_completed_stores_remain_visible_when_later_access_fails():
    harness = _Harness("st.b r4, r5\naddi r4, 8\nstr r4, r5\nret.l", inputs=2)
    result = harness.run((EXT_BASE, 0xA5), [(EXT_BASE, 8, "write")])
    assert result.exit_kind == "rejected_access"
    assert result.instructions == 2
    assert result.outputs == ()
    assert bytes(harness.external) == b"\xa5" + bytes(EXT_SIZE - 1)
    assert result.access_address == EXT_BASE + 8


def test_overlapping_borrowed_buffers_observe_instruction_order():
    harness = _Harness("str r4, r5\nldn r4, r4\nret.l", inputs=2, outputs=1)
    result = harness.run((EXT_BASE, 0x1234), [
        (EXT_BASE, 8, "write"), (EXT_BASE, 16, "read"),
    ])
    assert result.exit_kind == "returned"
    assert result.outputs == (0x1234,)
    assert int.from_bytes(harness.external[:8], "little") == 0x1234


@pytest.mark.parametrize("program", [
    "sep r3", "sex r2", "out1", "inp1", "halt", "idl", "ei",
    "inc r15", "mov r15, r4", "ldn r15, r4",
])
def test_unadmitted_operations_stop_before_their_effects(program):
    harness = _Harness(program + "\nret.l", inputs=1)
    result = harness.run((EXT_BASE,), [(EXT_BASE, 8, "read_write")])
    assert result.exit_kind == "unsupported_instruction"
    assert result.instructions == 0
    assert result.outputs == ()
    assert (harness.state.psel, harness.state.xsel, harness.state.spsel) == (3, 2, 15)
    assert harness.state.get_reg(15) == CONTROL_BASE + CONTROL_SIZE - 8
    assert harness.state.flag_i == 0
    assert bytes(harness.external) == bytes(EXT_SIZE)


@pytest.mark.parametrize("program", [
    "csrr r4, 0", "csrw 0, r4", "fadd.d r4, r5", "sha.init 0",
])
def test_service_and_extension_instructions_never_enter_general_execution(program):
    harness = _Harness(program + "\nret.l", inputs=2)
    result = harness.run((17, 23))
    assert result.exit_kind == "unsupported_instruction"
    assert result.instructions == 0
    assert (harness.state.get_reg(4), harness.state.get_reg(5)) == (17, 23)
    assert result.outputs == ()


def test_illegal_prefix_reports_decode_fault_without_completing_instruction():
    harness = _Harness(b"\xfd")
    result = harness.run()
    assert result.exit_kind == "decode_fault"
    assert result.instructions == 0
    assert result.instruction_pc == CODE_BASE
    assert result.outputs == ()


def test_instruction_operand_fetch_cannot_escape_sealed_code_span():
    # LDI64 starts on the final sealed byte; its operands remain mapped but
    # belong to no admitted executable image.
    harness = _Harness(b"\x01" * 15 + b"\xf0")
    spec = native.RoutineSpecV1(CODE_BASE, 16, 15, 0, 0,
                                CONTROL_BASE, CONTROL_SIZE, 100)
    result = harness.runner.run(spec, (), (), 10)
    assert result.exit_kind == "rejected_access"
    assert result.instructions == 0
    assert result.instruction_pc == CODE_BASE + 15
    assert result.access_address == CODE_BASE + 16


@pytest.mark.parametrize("span", [
    (EXT_BASE + EXT_SIZE - 1, 2, "read"), (MASK64, 2, "read"),
    (MMIO_BASE, 1, "read"), (RAM_SIZE + 0x800, 1, "write"),
    (CONTROL_BASE, 8, "read"), (CODE_BASE, 16, "write"),
    (EXT_BASE, 8, "execute"), (True, 1, "read"),
])
def test_invalid_borrow_preflight_has_no_register_or_control_effects(span):
    harness = _Harness()
    harness.state.set_reg(4, 0xCAFE)
    before = tuple(harness.state.get_reg(index) for index in range(32)), bytes(harness.control)
    expected_error = TypeError if isinstance(span[0], bool) else ValueError
    with pytest.raises(expected_error):
        harness.run(spans=[span])
    assert (tuple(harness.state.get_reg(index) for index in range(32)),
            bytes(harness.control)) == before


def test_protected_spans_reject_borrows_before_entry():
    harness = _Harness()
    with pytest.raises(ValueError):
        harness.run(spans=[(EXT_BASE, 16, "read")],
                    protected_spans=[(EXT_BASE + 8, 8)])
    assert bytes(harness.control) == bytes(CONTROL_SIZE)


@pytest.mark.parametrize("arguments,limit,error", [
    ((1,), 1, ValueError), ((), 0, ValueError),
    ((), True, TypeError), ((), 10000001, ValueError),
])
def test_argument_and_budget_preflight_precedes_machine_mutation(arguments, limit, error):
    harness = _Harness()
    with pytest.raises(error):
        harness.run(arguments, limit=limit)
    assert bytes(harness.control) == bytes(CONTROL_SIZE)


@pytest.mark.parametrize("argument,error", [
    (True, TypeError), ("1", TypeError), (-1, ValueError), (1 << 64, ValueError),
])
def test_argument_cells_require_exact_uint64_values(argument, error):
    harness = _Harness(inputs=1)
    with pytest.raises(error):
        harness.run((argument,))
    assert bytes(harness.control) == bytes(CONTROL_SIZE)


def test_cancelled_entry_completes_no_instructions_or_shared_stores():
    harness = _Harness("st.b r4, r5\nret.l", inputs=2)
    result = harness.run((EXT_BASE, 42), [(EXT_BASE, 1, "write")], cancelled=True)
    assert result.exit_kind == "cancelled"
    assert result.instructions == 0
    assert result.cycles == 0
    assert result.outputs == ()
    assert bytes(harness.external) == bytes(EXT_SIZE)


def test_cache_keeps_unpublished_bytes_and_publication_preserves_other_lines():
    harness = _Harness("ldi r4, 1\nret.l", outputs=1)
    other_base = CODE_BASE + 0x40
    other_image = _image("ldi r4, 7\nret.l", base=other_base)
    harness.ram[other_base:other_base + len(other_image)] = other_image
    other_spec = native.RoutineSpecV1(other_base, len(other_image), 0, 0, 1,
                                      CONTROL_BASE, CONTROL_SIZE, 100)
    harness.runner.publish_code(other_spec)
    assert harness.run().outputs == (1,)
    assert harness.runner.run(other_spec, (), (), 100).outputs == (7,)

    harness.ram[CODE_BASE:CODE_BASE + len(harness.image)] = _image("ldi r4, 2\nret.l")
    harness.ram[other_base:other_base + len(other_image)] = _image("ldi r4, 8\nret.l")
    assert harness.run().outputs == (1,)
    harness.runner.publish_code(harness.spec)
    assert harness.run().outputs == (2,)
    assert harness.runner.run(other_spec, (), (), 100).outputs == (7,)


def test_repeated_calls_share_mutations_and_retain_buffer_lifetimes():
    harness = _Harness("ldn r5, r4\ninc r5\nstr r4, r5\nmov r4, r5\nret.l",
                       inputs=1, outputs=1)
    spans = [(EXT_BASE, 8, "read_write")]
    assert harness.run((EXT_BASE,), spans).outputs == (1,)
    harness.external[:8] = (99).to_bytes(8, "little")
    state = harness.state
    del harness.state
    del state
    gc.collect()
    assert harness.run((EXT_BASE,), spans).outputs == (100,)
    assert int.from_bytes(harness.external[:8], "little") == 100
    for buffer in (harness.ram, harness.external, harness.control):
        with pytest.raises(BufferError):
            buffer.extend(b"x")


def test_rebound_architectural_mapping_cannot_run_with_stale_qualification():
    harness = _Harness()
    replacement = bytearray(harness.ram)
    with pytest.raises(RuntimeError):
        harness.state.attach_mem(replacement, len(replacement))
    with pytest.raises(RuntimeError):
        harness.state.ext_mem_base = EXT_BASE + 0x1000
    assert bytes(harness.control) == bytes(CONTROL_SIZE)
    assert harness.run().exit_kind == "returned"


def test_runner_releases_mapping_configuration_pin_on_destruction():
    harness = _Harness()
    runner = harness.runner
    del harness.runner
    del runner
    gc.collect()
    replacement = bytearray(harness.ram)
    harness.state.attach_mem(replacement, len(replacement))
    # CPUState still owns its replacement export; the displaced Bank 0 and
    # runner-owned private control buffer no longer have native borrowers.
    harness.ram.extend(b"x")
    harness.control.extend(b"x")
    with pytest.raises(BufferError):
        replacement.extend(b"x")


@pytest.mark.parametrize("field,value,error", [
    ("code_base", CODE_BASE + 1, ValueError), ("code_size", 0, ValueError),
    ("code_size", 17, ValueError), ("code_size", (1 << 20) + 16, ValueError),
    ("entry_offset", 16, ValueError), ("input_cells", 9, ValueError),
    ("output_cells", True, TypeError), ("stack_size", 0, ValueError),
    ("stack_size", 65544, ValueError), ("max_instructions", 0, ValueError),
    ("max_instructions", 1000001, ValueError),
    ("stack_base", MASK64 - 7, ValueError),
])
def test_declaration_bounds_are_validated_as_exact_host_integers(field, value, error):
    parameters = dict(code_base=CODE_BASE, code_size=16, entry_offset=0,
                      input_cells=0, output_cells=0, stack_base=CONTROL_BASE,
                      stack_size=CONTROL_SIZE, max_instructions=1000)
    parameters[field] = value
    with pytest.raises(error):
        native.RoutineSpecV1(**parameters)


@pytest.mark.parametrize("base,buffer", [
    (CONTROL_BASE + 1, bytearray(CONTROL_SIZE)),
    (MASK64 - 7, bytearray(16)),
    (MMIO_BASE, bytearray(CONTROL_SIZE)),
    (CODE_BASE, bytearray(CONTROL_SIZE)),
    (CONTROL_BASE, bytes(CONTROL_SIZE)),
])
def test_private_control_buffer_requires_separate_writable_bounded_storage(base, buffer):
    state = native.CPUState()
    state.attach_mem(bytearray(RAM_SIZE), RAM_SIZE)
    with pytest.raises((TypeError, ValueError, BufferError)):
        native.RoutineRunnerV1(state, base, buffer)


@pytest.mark.parametrize("alias_kind", ["control", "ordinary_region"])
def test_distinct_guest_regions_cannot_alias_the_same_host_bytes(alias_kind):
    ram = bytearray(RAM_SIZE)
    state = native.CPUState()
    state.attach_mem(ram, len(ram))
    if alias_kind == "ordinary_region":
        state.attach_ext_mem(memoryview(ram)[:EXT_SIZE], EXT_BASE, EXT_SIZE)
        control = bytearray(CONTROL_SIZE)
    else:
        control = memoryview(ram)[:CONTROL_SIZE]
    with pytest.raises(ValueError):
        native.RoutineRunnerV1(state, CONTROL_BASE, control)
