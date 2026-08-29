#include "emitter.h"

#include <limits>
#include <stdexcept>
#include <utility>

void X86_64BlockEmitter::byte(uint8_t value) {
    code_.push_back(value);
}

void X86_64BlockEmitter::u32(uint32_t value) {
    for (int shift = 0; shift < 32; shift += 8)
        byte(static_cast<uint8_t>(value >> shift));
}

void X86_64BlockEmitter::i32(int32_t value) {
    u32(static_cast<uint32_t>(value));
}

void X86_64BlockEmitter::u64(uint64_t value) {
    u32(static_cast<uint32_t>(value));
    u32(static_cast<uint32_t>(value >> 32));
}

std::size_t X86_64BlockEmitter::position() const noexcept {
    return code_.size();
}

std::size_t X86_64BlockEmitter::branch32(uint8_t condition) {
    byte(0x0F);
    byte(condition);
    const std::size_t displacement = position();
    u32(0);
    return displacement;
}

std::size_t X86_64BlockEmitter::jump32() {
    byte(0xE9);
    const std::size_t displacement = position();
    u32(0);
    return displacement;
}

void X86_64BlockEmitter::patch32(
        std::size_t displacement,
        std::size_t target) {
    if (
        displacement + 4 > code_.size() ||
        target > code_.size() ||
        target > static_cast<std::size_t>(
            std::numeric_limits<int32_t>::max())
    ) {
        throw std::logic_error(
            "x86-64 JIT branch target is out of range");
    }
    const int64_t relative =
        static_cast<int64_t>(target) -
        static_cast<int64_t>(displacement + 4);
    if (
        relative < std::numeric_limits<int32_t>::min() ||
        relative > std::numeric_limits<int32_t>::max()
    ) {
        throw std::logic_error(
            "x86-64 JIT branch displacement is out of range");
    }
    const uint32_t encoded = static_cast<uint32_t>(
        static_cast<int32_t>(relative));
    for (int index = 0; index < 4; index++) {
        code_[displacement + static_cast<std::size_t>(index)] =
            static_cast<uint8_t>(encoded >> (index * 8));
    }
}

void X86_64BlockEmitter::mov_rax_from_core(int32_t displacement) {
    bytes({0x48, 0x8B, 0x87});
    i32(displacement);
}

void X86_64BlockEmitter::mov_rcx_from_core(int32_t displacement) {
    bytes({0x48, 0x8B, 0x8F});
    i32(displacement);
}

void X86_64BlockEmitter::mov_r9_from_core(int32_t displacement) {
    bytes({0x4C, 0x8B, 0x8F});
    i32(displacement);
}

void X86_64BlockEmitter::mov_r9_immediate(uint64_t immediate) {
    if (immediate <= std::numeric_limits<uint32_t>::max()) {
        bytes({0x41, 0xB9}); // mov r9d, imm32 (zero extending)
        u32(static_cast<uint32_t>(immediate));
    } else {
        bytes({0x49, 0xB9}); // movabs r9, imm64
        u64(immediate);
    }
}

void X86_64BlockEmitter::mov_r8_from_pointer_table(
        std::size_t index) {
    if (
        index > static_cast<std::size_t>(
            std::numeric_limits<int32_t>::max()) / sizeof(void*)
    ) {
        throw std::logic_error(
            "x86-64 pointer-table index exceeds displacement range");
    }
    // The third SysV argument is the table base in RDX. R8 is caller-saved
    // and otherwise unused by generated MP64 blocks.
    if (index == 0) {
        bytes({0x4C, 0x8B, 0x02}); // mov r8, [rdx]
    } else if (index <= 15) {
        bytes({
            0x4C,
            0x8B,
            0x42,
            static_cast<uint8_t>(index * sizeof(void*)),
        }); // mov r8, [rdx + disp8]
    } else {
        bytes({0x4C, 0x8B, 0x82});
        i32(static_cast<int32_t>(index * sizeof(void*)));
        // mov r8, [rdx + disp32]
    }
}

void X86_64BlockEmitter::mov_core_from_rax(int32_t displacement) {
    bytes({0x48, 0x89, 0x87});
    i32(displacement);
}

void X86_64BlockEmitter::mov_core_from_rcx(int32_t displacement) {
    bytes({0x48, 0x89, 0x8F});
    i32(displacement);
}

void X86_64BlockEmitter::mov_core_from_r9(int32_t displacement) {
    bytes({0x4C, 0x89, 0x8F});
    i32(displacement);
}

void X86_64BlockEmitter::imul_rax_from_core(int32_t displacement) {
    // The low half of signed and unsigned multiplication is identical. This
    // two-operand form preserves RDX, which carries the pointer-table ABI.
    bytes({0x48, 0x0F, 0xAF, 0x87});
    i32(displacement);
}

void X86_64BlockEmitter::add_r9_imm8(uint8_t immediate) {
    bytes({0x49, 0x83, 0xC1, immediate});
}

void X86_64BlockEmitter::add_r9_imm32(uint32_t immediate) {
    bytes({0x49, 0x81, 0xC1});
    u32(immediate);
}

void X86_64BlockEmitter::add_core_imm8(
        int32_t displacement,
        uint8_t immediate) {
    bytes({0x48, 0x83, 0x87});
    i32(displacement);
    byte(immediate);
}

void X86_64BlockEmitter::add_core_imm32(
        int32_t displacement,
        uint32_t immediate) {
    bytes({0x48, 0x81, 0x87});
    i32(displacement);
    u32(immediate);
}

void X86_64BlockEmitter::sub_core_imm8(
        int32_t displacement,
        uint8_t immediate) {
    bytes({0x48, 0x83, 0xAF});
    i32(displacement);
    byte(immediate);
}

void X86_64BlockEmitter::increment_core(int32_t displacement) {
    bytes({0x48, 0xFF, 0x87});
    i32(displacement);
}

void X86_64BlockEmitter::decrement_core(int32_t displacement) {
    bytes({0x48, 0xFF, 0x8F});
    i32(displacement);
}

void X86_64BlockEmitter::add_core_r15(int32_t displacement) {
    bytes({0x4C, 0x01, 0xBF});
    i32(displacement);
}

void X86_64BlockEmitter::compare_core_byte(
        int32_t displacement,
        uint8_t value) {
    bytes({0x80, 0xBF});
    i32(displacement);
    byte(value);
}

void X86_64BlockEmitter::compare_core_qword_immediate(
        int32_t displacement,
        uint64_t value) {
    bytes({0x48, 0xB8}); // movabs rax, imm64
    u64(value);
    bytes({0x48, 0x39, 0x87}); // cmp qword ptr [rdi + disp32], rax
    i32(displacement);
}

void X86_64BlockEmitter::compare_r9_immediate(uint64_t value) {
    bytes({0x48, 0xB8}); // movabs rax, imm64
    u64(value);
    bytes({0x49, 0x39, 0xC1}); // cmp r9, rax
}

void X86_64BlockEmitter::add_exit_flags(uint8_t flags) {
    bytes({0x41, 0x83, 0xCA, flags});
}

void X86_64BlockEmitter::store_core_byte(
        int32_t displacement,
        uint8_t value) {
    bytes({0xC6, 0x87});
    i32(displacement);
    byte(value);
}

void X86_64BlockEmitter::store_core_qword_zero(
        int32_t displacement) {
    bytes({0x48, 0xC7, 0x87});
    i32(displacement);
    u32(0);
}

void X86_64BlockEmitter::set_core_byte(
        uint8_t condition_opcode,
        int32_t displacement) {
    bytes({0x0F, condition_opcode, 0x87});
    i32(displacement);
}

void X86_64BlockEmitter::bytes(
        std::initializer_list<uint8_t> values) {
    code_.insert(code_.end(), values.begin(), values.end());
}

std::vector<uint8_t> X86_64BlockEmitter::release_code() noexcept {
    return std::move(code_);
}
