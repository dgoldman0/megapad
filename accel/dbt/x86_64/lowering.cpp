#include "lowering.h"

#include <atomic>
#include <limits>
#include <stdexcept>
#include <vector>

#include "../host_jit_config.h"
#include "emitter.h"
#include "../../cpu/mp64/semantics.h"

namespace mp64::dbt::x86_64 {
namespace {

using cpu::CC_AL;
using cpu::CC_CC;
using cpu::CC_EQ;
using cpu::CC_NE;
using cpu::DecodedInstruction;
using cpu::DecodedOperation;

#if MP64_HAS_X86_64_JIT
static_assert(
    sizeof(std::atomic<uint8_t>) == sizeof(uint8_t) &&
        std::atomic<uint8_t>::is_always_lock_free,
    "x86-64 JIT interrupt polling requires a lock-free byte atomic");
static_assert(
    sizeof(void*) == 8 && sizeof(std::size_t) == 8 &&
        sizeof(bool) == 1,
    "x86-64 JIT requires the 64-bit SysV data model");
#endif

struct BytePredicate {
    int32_t displacement = 0;
    uint8_t expected = 0;
    bool supported = false;
};

void validate_layout(const CoreStateLayout& layout) {
    const auto span_fits = [&layout](
            int32_t displacement,
            std::size_t width) noexcept {
        if (displacement < 0)
            return false;
        const std::size_t offset =
            static_cast<std::size_t>(displacement);
        return
            offset <= layout.object_size &&
            width <= layout.object_size - offset;
    };
    for (const int32_t displacement : layout.registers) {
        if (!span_fits(displacement, sizeof(uint64_t))) {
            throw std::logic_error(
                "x86-64 JIT register layout exceeds CPU state");
        }
    }
    if (
        !span_fits(layout.program_counter_selector, sizeof(uint8_t)) ||
        !span_fits(layout.flag_z, sizeof(uint8_t)) ||
        !span_fits(layout.flag_c, sizeof(uint8_t)) ||
        !span_fits(layout.flag_n, sizeof(uint8_t)) ||
        !span_fits(layout.flag_v, sizeof(uint8_t)) ||
        !span_fits(layout.flag_p, sizeof(uint8_t)) ||
        !span_fits(layout.flag_g, sizeof(uint8_t)) ||
        !span_fits(layout.icache_hits, sizeof(uint64_t)) ||
        !span_fits(layout.icache_misses, sizeof(uint64_t)) ||
        !span_fits(layout.ifetch_window_valid, sizeof(uint8_t)) ||
        !span_fits(layout.icache_undo_count, sizeof(uint64_t)) ||
        !span_fits(layout.icache_undo_hits, sizeof(uint64_t)) ||
        !span_fits(layout.icache_undo_misses, sizeof(uint64_t))
    ) {
        throw std::logic_error(
            "x86-64 JIT layout exceeds CPU state");
    }
}

BytePredicate byte_predicate(
        const CoreStateLayout& layout,
        uint8_t subop) {
    switch (subop) {
        case CC_EQ:
            return {layout.flag_z, 1, true};
        case CC_NE:
            return {layout.flag_z, 0, true};
        case CC_CC:
            return {layout.flag_c, 0, true};
        default:
            return {};
    }
}

void emit_logic_flags(
        X86_64BlockEmitter& emitter,
        const CoreStateLayout& layout) {
    emitter.set_core_byte(0x94, layout.flag_z);
    emitter.set_core_byte(0x98, layout.flag_n);
    emitter.set_core_byte(0x9A, layout.flag_p);
    emitter.store_core_byte(layout.flag_c, 0);
    emitter.store_core_byte(layout.flag_v, 0);
}

void emit_addition_flags(
        X86_64BlockEmitter& emitter,
        const CoreStateLayout& layout) {
    emitter.set_core_byte(0x94, layout.flag_z);
    emitter.set_core_byte(0x92, layout.flag_c);
    emitter.set_core_byte(0x98, layout.flag_n);
    emitter.set_core_byte(0x90, layout.flag_v);
    emitter.set_core_byte(0x9A, layout.flag_p);
}

void emit_subtraction_flags(
        X86_64BlockEmitter& emitter,
        const CoreStateLayout& layout) {
    emitter.set_core_byte(0x94, layout.flag_z);
    // x86 CF means borrow after SUB; the guest C flag means no borrow.
    emitter.set_core_byte(0x93, layout.flag_c);
    emitter.set_core_byte(0x98, layout.flag_n);
    emitter.set_core_byte(0x90, layout.flag_v);
    emitter.set_core_byte(0x9A, layout.flag_p);
}

void emit_comparison_flags(
        X86_64BlockEmitter& emitter,
        const CoreStateLayout& layout) {
    emit_subtraction_flags(emitter, layout);
    // Guest G is unsigned greater-than, matching x86 SETA after CMP.
    emitter.set_core_byte(0x97, layout.flag_g);
}

bool operation_uses_program_counter_operand(
        const DecodedInstruction& decoded,
        uint8_t psel,
        uint8_t spsel) noexcept {
    switch (decoded.operation) {
        case DecodedOperation::NOP:
        case DecodedOperation::BRANCH_SHORT:
        case DecodedOperation::BRANCH_LONG:
        case DecodedOperation::SELECT_PROGRAM_COUNTER:
            return false;
        case DecodedOperation::CALL_LONG:
            return
                spsel == psel ||
                decoded.rs == psel ||
                decoded.rs == spsel;
        case DecodedOperation::RETURN_LONG:
            return spsel == psel;
        case DecodedOperation::INCREMENT:
        case DecodedOperation::DECREMENT:
        case DecodedOperation::LOAD_IMMEDIATE:
        case DecodedOperation::ADD_IMMEDIATE:
        case DecodedOperation::AND_IMMEDIATE:
        case DecodedOperation::OR_IMMEDIATE:
        case DecodedOperation::COMPARE_IMMEDIATE:
        case DecodedOperation::SUBTRACT_IMMEDIATE:
        case DecodedOperation::SHIFT_LEFT_IMMEDIATE:
        case DecodedOperation::SHIFT_RIGHT_LOGICAL_IMMEDIATE:
        case DecodedOperation::ROTATE_LEFT_IMMEDIATE:
            return decoded.rd == psel;
        case DecodedOperation::LOAD_NATURAL:
        case DecodedOperation::LOAD_BYTE:
        case DecodedOperation::STORE_NATURAL:
        case DecodedOperation::STORE_BYTE:
        case DecodedOperation::ADD:
        case DecodedOperation::SUBTRACT:
        case DecodedOperation::BITWISE_XOR:
        case DecodedOperation::COMPARE:
        case DecodedOperation::MOVE:
            return decoded.rd == psel || decoded.rs == psel;
        default:
            return true;
    }
}

bool should_keep_program_counter_live(const BlockView& block) noexcept {
    for (std::size_t index = 0; index < block.instruction_count; index++) {
        if (operation_uses_program_counter_operand(
                block.instructions[index],
                block.psel,
                block.spsel)) {
            return false;
        }
    }

    // A core-memory PC adjustment is nine bytes; the R9 form is four. Entry
    // address materialization plus the one common spill costs fourteen bytes
    // below 4 GiB and eighteen above it. Control transfers avoid one more PC
    // adjustment or replacement. Select the mode only when its emitted shape
    // is strictly smaller, rather than imposing a separate block-size rule.
    int64_t saved_bytes =
        static_cast<int64_t>(block.instruction_count) * 5 -
        (
            block.address <= std::numeric_limits<uint32_t>::max()
                ? 14
                : 18
        );
    const DecodedOperation terminal =
        block.instructions[block.instruction_count - 1].operation;
    if (
        terminal == DecodedOperation::BRANCH_SHORT ||
        terminal == DecodedOperation::BRANCH_LONG
    ) {
        saved_bytes += 5;
    } else if (terminal == DecodedOperation::CALL_LONG) {
        saved_bytes += 16;
    } else if (terminal == DecodedOperation::RETURN_LONG) {
        saved_bytes += 8;
    }
    return saved_bytes > 0;
}

void emit_program_counter_add_imm8(
        X86_64BlockEmitter& emitter,
        const CoreStateLayout& layout,
        uint8_t psel,
        uint8_t immediate,
        bool program_counter_live) {
    if (program_counter_live) {
        emitter.add_r9_imm8(immediate);
    } else {
        emitter.add_core_imm8(
            layout.registers[psel],
            immediate);
    }
}

void emit_program_counter_add_imm32(
        X86_64BlockEmitter& emitter,
        const CoreStateLayout& layout,
        uint8_t psel,
        uint32_t immediate,
        bool program_counter_live) {
    if (program_counter_live) {
        emitter.add_r9_imm32(immediate);
    } else {
        emitter.add_core_imm32(
            layout.registers[psel],
            immediate);
    }
}

void emit_instruction(
        X86_64BlockEmitter& emitter,
        const CoreStateLayout& layout,
        const DecodedInstruction& decoded,
        uint64_t instruction_address,
        uint8_t psel,
        uint8_t spsel,
        std::size_t memory_access_index,
        bool program_counter_live) {
    emit_program_counter_add_imm8(
        emitter,
        layout,
        psel,
        decoded.encoded_size,
        program_counter_live);
    emitter.bytes({
        0x41,
        0x83,
        0xC7,
        decoded.fetch_hit_count(instruction_address),
    });

    switch (decoded.operation) {
        case DecodedOperation::NOP:
            return;
        case DecodedOperation::CALL_LONG:
            emitter.mov_r8_from_pointer_table(memory_access_index);
            if (program_counter_live) {
                emitter.sub_core_imm8(layout.registers[spsel], 8);
                emitter.bytes({0x4D, 0x89, 0x08}); // mov [r8], r9
                emitter.mov_r9_from_core(layout.registers[decoded.rs]);
            } else {
                emitter.mov_rcx_from_core(layout.registers[decoded.rs]);
                emitter.mov_rax_from_core(layout.registers[psel]);
                emitter.sub_core_imm8(layout.registers[spsel], 8);
                emitter.bytes({0x49, 0x89, 0x00}); // mov [r8], rax
                emitter.mov_core_from_rcx(layout.registers[psel]);
            }
            return;
        case DecodedOperation::RETURN_LONG:
            emitter.mov_r8_from_pointer_table(memory_access_index);
            if (program_counter_live) {
                emitter.bytes({0x4D, 0x8B, 0x08}); // mov r9, [r8]
                emitter.add_core_imm8(layout.registers[spsel], 8);
            } else {
                emitter.bytes({0x49, 0x8B, 0x00}); // mov rax, [r8]
                emitter.add_core_imm8(layout.registers[spsel], 8);
                emitter.mov_core_from_rax(layout.registers[psel]);
            }
            return;
        case DecodedOperation::INCREMENT:
            emitter.increment_core(layout.registers[decoded.rd]);
            return;
        case DecodedOperation::DECREMENT:
            emitter.decrement_core(layout.registers[decoded.rd]);
            return;
        case DecodedOperation::BRANCH_SHORT:
            if (decoded.subop() == CC_AL) {
                emit_program_counter_add_imm8(
                    emitter,
                    layout,
                    psel,
                    static_cast<uint8_t>(decoded.immediate),
                    program_counter_live);
                return;
            }
            if (
                (
                    decoded.subop() != CC_EQ &&
                    decoded.subop() != CC_NE &&
                    decoded.subop() != CC_CC
                ) ||
                decoded.conditional_taken_cycle_cost() != 1
            ) {
                throw std::logic_error(
                    "x86-64 JIT received an unsupported short branch");
            }
            {
                const BytePredicate predicate =
                    byte_predicate(layout, decoded.subop());
                if (!predicate.supported) {
                    throw std::logic_error(
                        "x86-64 JIT received an unsupported short branch");
                }
                emitter.compare_core_byte(
                    predicate.displacement,
                    predicate.expected);
            }
            {
                const std::size_t not_taken =
                    emitter.branch32(0x85); // jne
                emit_program_counter_add_imm8(
                    emitter,
                    layout,
                    psel,
                    static_cast<uint8_t>(decoded.immediate),
                    program_counter_live);
                emitter.add_exit_flags(
                    NATIVE_BLOCK_EXIT_CONDITIONAL_TAKEN);
                emitter.patch32(not_taken, emitter.position());
            }
            return;
        case DecodedOperation::BRANCH_LONG:
            if (decoded.subop() == CC_AL) {
                emit_program_counter_add_imm32(
                    emitter,
                    layout,
                    psel,
                    static_cast<uint32_t>(
                        cpu::sign_extend(decoded.immediate, 16)),
                    program_counter_live);
                return;
            }
            if (
                (
                    decoded.subop() != CC_EQ &&
                    decoded.subop() != CC_NE &&
                    decoded.subop() != CC_CC
                ) ||
                decoded.conditional_taken_cycle_cost() != 1
            ) {
                throw std::logic_error(
                    "x86-64 JIT received an unsupported long branch");
            }
            {
                const BytePredicate predicate =
                    byte_predicate(layout, decoded.subop());
                if (!predicate.supported) {
                    throw std::logic_error(
                        "x86-64 JIT received an unsupported long branch");
                }
                emitter.compare_core_byte(
                    predicate.displacement,
                    predicate.expected);
            }
            {
                const std::size_t not_taken =
                    emitter.branch32(0x85); // jne
                emit_program_counter_add_imm32(
                    emitter,
                    layout,
                    psel,
                    static_cast<uint32_t>(
                        cpu::sign_extend(decoded.immediate, 16)),
                    program_counter_live);
                emitter.add_exit_flags(
                    NATIVE_BLOCK_EXIT_CONDITIONAL_TAKEN);
                emitter.patch32(not_taken, emitter.position());
            }
            return;
        case DecodedOperation::LOAD_NATURAL:
        case DecodedOperation::LOAD_BYTE:
        case DecodedOperation::STORE_NATURAL:
        case DecodedOperation::STORE_BYTE:
            // The third SysV argument is an ordered table of pointers proved
            // before entry. Keep address selection out of generated guest
            // semantics while allowing more than one admitted span.
            emitter.mov_r8_from_pointer_table(memory_access_index);
            if (decoded.operation == DecodedOperation::LOAD_NATURAL) {
                emitter.bytes({0x49, 0x8B, 0x00}); // mov rax, [r8]
            } else if (decoded.operation == DecodedOperation::LOAD_BYTE) {
                emitter.bytes({
                    0x41, 0x0F, 0xB6, 0x00,
                }); // movzx eax, byte [r8]
            } else {
                emitter.mov_rax_from_core(layout.registers[decoded.rs]);
                if (decoded.operation == DecodedOperation::STORE_NATURAL) {
                    emitter.bytes({0x49, 0x89, 0x00}); // mov [r8], rax
                } else {
                    emitter.bytes({0x41, 0x88, 0x00}); // mov byte [r8], al
                }
                return;
            }
            emitter.mov_core_from_rax(layout.registers[decoded.rd]);
            return;
        case DecodedOperation::LOAD_IMMEDIATE:
        case DecodedOperation::ADD_IMMEDIATE:
        case DecodedOperation::AND_IMMEDIATE:
        case DecodedOperation::OR_IMMEDIATE:
        case DecodedOperation::COMPARE_IMMEDIATE:
        case DecodedOperation::SUBTRACT_IMMEDIATE:
        case DecodedOperation::SHIFT_LEFT_IMMEDIATE:
        case DecodedOperation::SHIFT_RIGHT_LOGICAL_IMMEDIATE:
        case DecodedOperation::ROTATE_LEFT_IMMEDIATE:
            if (decoded.operation == DecodedOperation::LOAD_IMMEDIATE) {
                if (decoded.encoded_size == 3) {
                    // The unprefixed form has an imm8, so writing EAX gives
                    // the required full-register zero extension compactly.
                    emitter.byte(0xB8); // mov eax, imm32
                    emitter.u32(static_cast<uint32_t>(decoded.immediate));
                } else if (decoded.encoded_size == 11) {
                    emitter.bytes({0x48, 0xB8}); // movabs rax, imm64
                    emitter.u64(decoded.immediate);
                } else {
                    throw std::logic_error(
                        "x86-64 JIT received an invalid LDI encoding");
                }
                emitter.mov_core_from_rax(layout.registers[decoded.rd]);
                return;
            }
            emitter.mov_rax_from_core(layout.registers[decoded.rd]);
            switch (decoded.operation) {
                case DecodedOperation::ADD_IMMEDIATE:
                    emitter.bytes({
                        0x48,
                        0x83,
                        0xC0,
                        static_cast<uint8_t>(decoded.immediate),
                    });
                    emitter.mov_core_from_rax(layout.registers[decoded.rd]);
                    emit_addition_flags(emitter, layout);
                    return;
                case DecodedOperation::AND_IMMEDIATE:
                    // AND EAX with a zero-extended imm32. The guest mask is
                    // an imm8; opcode 83 would sign-extend 0x80..0xff and
                    // incorrectly retain high register bits.
                    emitter.byte(0x25);
                    emitter.u32(static_cast<uint32_t>(decoded.immediate));
                    emitter.mov_core_from_rax(layout.registers[decoded.rd]);
                    emit_logic_flags(emitter, layout);
                    return;
                case DecodedOperation::OR_IMMEDIATE:
                    emitter.bytes({0x48, 0x0D});
                    emitter.u32(static_cast<uint32_t>(decoded.immediate));
                    emitter.mov_core_from_rax(layout.registers[decoded.rd]);
                    emit_logic_flags(emitter, layout);
                    return;
                case DecodedOperation::COMPARE_IMMEDIATE:
                    emitter.bytes({
                        0x48,
                        0x83,
                        0xF8,
                        static_cast<uint8_t>(decoded.immediate),
                    });
                    emit_comparison_flags(emitter, layout);
                    return;
                case DecodedOperation::SUBTRACT_IMMEDIATE:
                    emitter.bytes({
                        0x48,
                        0x83,
                        0xE8,
                        static_cast<uint8_t>(decoded.immediate),
                    });
                    emitter.mov_core_from_rax(layout.registers[decoded.rd]);
                    emit_subtraction_flags(emitter, layout);
                    return;
                case DecodedOperation::SHIFT_LEFT_IMMEDIATE:
                case DecodedOperation::SHIFT_RIGHT_LOGICAL_IMMEDIATE: {
                    const uint8_t modrm =
                        decoded.operation ==
                                DecodedOperation::SHIFT_LEFT_IMMEDIATE
                            ? 0xE0
                            : 0xE8;
                    emitter.bytes({
                        0x48,
                        0xC1,
                        modrm,
                        static_cast<uint8_t>(decoded.immediate),
                    });
                    emitter.mov_core_from_rax(layout.registers[decoded.rd]);
                    return;
                }
                case DecodedOperation::ROTATE_LEFT_IMMEDIATE:
                    emitter.bytes({
                        0x48,
                        0xC1,
                        0xC0,
                        static_cast<uint8_t>(decoded.immediate & 0xF),
                    });
                    emitter.mov_core_from_rax(layout.registers[decoded.rd]);
                    return;
                default:
                    throw std::logic_error(
                        "x86-64 JIT received an unsupported immediate op");
            }
        case DecodedOperation::ADD:
        case DecodedOperation::SUBTRACT:
        case DecodedOperation::BITWISE_XOR:
        case DecodedOperation::COMPARE:
        case DecodedOperation::MOVE:
            emitter.mov_rax_from_core(layout.registers[decoded.rd]);
            emitter.mov_rcx_from_core(layout.registers[decoded.rs]);
            switch (decoded.operation) {
                case DecodedOperation::ADD:
                    emitter.bytes({0x48, 0x01, 0xC8});
                    emitter.mov_core_from_rax(layout.registers[decoded.rd]);
                    emit_addition_flags(emitter, layout);
                    return;
                case DecodedOperation::SUBTRACT:
                    emitter.bytes({0x48, 0x29, 0xC8});
                    emitter.mov_core_from_rax(layout.registers[decoded.rd]);
                    emit_subtraction_flags(emitter, layout);
                    return;
                case DecodedOperation::BITWISE_XOR:
                    emitter.bytes({0x48, 0x31, 0xC8});
                    emitter.mov_core_from_rax(layout.registers[decoded.rd]);
                    emit_logic_flags(emitter, layout);
                    return;
                case DecodedOperation::COMPARE:
                    emitter.bytes({0x48, 0x39, 0xC8});
                    emit_comparison_flags(emitter, layout);
                    return;
                case DecodedOperation::MOVE:
                    emitter.bytes({0x49, 0x89, 0x8C, 0x24});
                    emitter.i32(layout.registers[decoded.rd]);
                    return;
                default:
                    throw std::logic_error(
                        "x86-64 JIT received an unsupported ALU op");
            }
        case DecodedOperation::SELECT_PROGRAM_COUNTER:
            emitter.store_core_byte(
                layout.program_counter_selector,
                decoded.rd);
            return;
        default:
            throw std::logic_error(
                "x86-64 JIT received an unsupported semantic operation");
    }
}

}  // namespace

bool lowering_available() noexcept {
    return MP64_HAS_X86_64_JIT != 0;
}

std::vector<uint8_t> lower_block(
        const CoreStateLayout& layout,
        const BlockView& block) {
    if (!lowering_available()) {
        throw std::logic_error(
            "x86-64 JIT lowering is unavailable on this host");
    }
    if (
        block.instructions == nullptr ||
        block.instruction_count == 0 ||
        block.instruction_count >
            static_cast<std::size_t>(
                std::numeric_limits<uint16_t>::max()) ||
        block.psel >= layout.registers.size() ||
        block.spsel >= layout.registers.size()
    ) {
        throw std::logic_error(
            "x86-64 JIT received an invalid MP64 block view");
    }
    validate_layout(layout);
    const bool program_counter_live =
        should_keep_program_counter_live(block);

    X86_64BlockEmitter emitter;
    // ENDBR64 is a no-op on hosts without CET and permits indirect entry
    // when control-flow enforcement is active.
    emitter.bytes({0xF3, 0x0F, 0x1E, 0xFA});
    emitter.bytes({
        0x53,             // push rbx
        0x41, 0x54,       // push r12
        0x41, 0x55,       // push r13
        0x41, 0x57,       // push r15
        0x49, 0x89, 0xFC, // mov r12, rdi (CPU state*)
        0x49, 0x89, 0xF5, // mov r13, rsi (enabled IPI mirror*)
        0x31, 0xDB,       // xor ebx, ebx (retired steps)
        0x45, 0x31, 0xFF, // xor r15d, r15d (fetch hits)
        0x45, 0x31, 0xD2, // xor r10d, r10d (exit flags)
    });
    if (program_counter_live)
        emitter.mov_r9_immediate(block.address);

    std::vector<std::size_t> interrupt_exits;
    uint64_t instruction_address = block.address;
    std::size_t memory_access_index = 0;
    for (std::size_t index = 0; index < block.instruction_count; index++) {
        const DecodedInstruction& decoded = block.instructions[index];
        if (
            decoded.rd >= layout.registers.size() ||
            decoded.rs >= layout.registers.size()
        ) {
            throw std::logic_error(
                "x86-64 JIT received an invalid MP64 register operand");
        }
        if (
            decoded.ends_block() &&
            index + 1 != block.instruction_count
        ) {
            throw std::logic_error(
                "x86-64 JIT received non-terminal control flow");
        }
        emit_instruction(
            emitter,
            layout,
            decoded,
            instruction_address,
            block.psel,
            block.spsel,
            memory_access_index,
            program_counter_live);
        if (decoded.is_memory())
            memory_access_index++;
        emitter.bytes({0xFF, 0xC3}); // inc ebx
        emitter.bytes({
            0x41,
            0xBB,
            decoded.fetch_hit_count(instruction_address),
            0x00,
            0x00,
            0x00,
        }); // mov r11d, last instruction's fetch hits
        instruction_address += decoded.encoded_size;

        if (index + 1 < block.instruction_count) {
            emitter.bytes({0x4D, 0x85, 0xED}); // test r13, r13
            const std::size_t interrupts_masked =
                emitter.branch32(0x84); // je
            emitter.bytes({
                0x41, 0x80, 0x7D, 0x00, 0x00,
            }); // cmp byte ptr [r13 atomic mirror], 0
            interrupt_exits.push_back(
                emitter.branch32(0x85)); // jne
            emitter.patch32(interrupts_masked, emitter.position());
        }
    }

    const std::size_t common_exit = emitter.position();
    if (program_counter_live) {
        emitter.mov_core_from_r9(
            layout.registers[block.psel]);
    }
    emitter.add_core_r15(layout.icache_hits);

    // Reproduce icache_begin_instruction() for the last retired instruction.
    // The block was resident, so misses do not change; undo_hits is the final
    // hit count less the last fetch's hits.
    emitter.mov_rax_from_core(layout.icache_hits);
    emitter.bytes({0x4C, 0x29, 0xD8}); // sub rax, r11
    emitter.mov_core_from_rax(layout.icache_undo_hits);
    emitter.mov_rax_from_core(layout.icache_misses);
    emitter.mov_core_from_rax(layout.icache_undo_misses);
    emitter.store_core_byte(layout.ifetch_window_valid, 0);
    emitter.store_core_qword_zero(layout.icache_undo_count);
    // Return retired steps in bits 0..15 and exit flags in bits 16..17.
    // C++ owns cycle/PERF accounting from publication metadata plus the live
    // conditional-taken result.
    emitter.bytes({
        0x89, 0xD8,       // mov eax, ebx (retired steps)
        0x49, 0xC1, 0xE2,
        NATIVE_BLOCK_EXIT_SHIFT, // shl r10, 16
        0x4C, 0x09, 0xD0, // or rax, r10
        0x41, 0x5F,       // pop r15
        0x41, 0x5D,       // pop r13
        0x41, 0x5C,       // pop r12
        0x5B,             // pop rbx
        0xC3,             // ret
    });

    const std::size_t interrupt_exit = emitter.position();
    for (const std::size_t branch : interrupt_exits)
        emitter.patch32(branch, interrupt_exit);
    emitter.bytes({
        0x41,
        0xBA,
        NATIVE_BLOCK_EXIT_INTERRUPT,
        0x00,
        0x00,
        0x00,
    }); // mov r10d, interrupt flag
    const std::size_t interrupt_to_common = emitter.jump32();
    emitter.patch32(interrupt_to_common, common_exit);

    return emitter.release_code();
}

}  // namespace mp64::dbt::x86_64
