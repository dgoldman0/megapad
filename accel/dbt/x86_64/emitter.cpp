#include "emitter.h"

#include <limits>
#include <stdexcept>

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
    bytes({0x49, 0x8B, 0x84, 0x24});
    i32(displacement);
}

void X86_64BlockEmitter::mov_rcx_from_core(int32_t displacement) {
    bytes({0x49, 0x8B, 0x8C, 0x24});
    i32(displacement);
}

void X86_64BlockEmitter::mov_core_from_rax(int32_t displacement) {
    bytes({0x49, 0x89, 0x84, 0x24});
    i32(displacement);
}

void X86_64BlockEmitter::mov_core_from_rcx(int32_t displacement) {
    bytes({0x49, 0x89, 0x8C, 0x24});
    i32(displacement);
}

void X86_64BlockEmitter::add_core_imm8(
        int32_t displacement,
        uint8_t immediate) {
    bytes({0x49, 0x83, 0x84, 0x24});
    i32(displacement);
    byte(immediate);
}

void X86_64BlockEmitter::add_core_imm32(
        int32_t displacement,
        uint32_t immediate) {
    bytes({0x49, 0x81, 0x84, 0x24});
    i32(displacement);
    u32(immediate);
}

void X86_64BlockEmitter::sub_core_imm8(
        int32_t displacement,
        uint8_t immediate) {
    bytes({0x49, 0x83, 0xAC, 0x24});
    i32(displacement);
    byte(immediate);
}

void X86_64BlockEmitter::increment_core(int32_t displacement) {
    bytes({0x49, 0xFF, 0x84, 0x24});
    i32(displacement);
}

void X86_64BlockEmitter::decrement_core(int32_t displacement) {
    bytes({0x49, 0xFF, 0x8C, 0x24});
    i32(displacement);
}

void X86_64BlockEmitter::add_core_r15(int32_t displacement) {
    bytes({0x4D, 0x01, 0xBC, 0x24});
    i32(displacement);
}

void X86_64BlockEmitter::compare_core_byte(
        int32_t displacement,
        uint8_t value) {
    bytes({0x41, 0x80, 0xBC, 0x24});
    i32(displacement);
    byte(value);
}

void X86_64BlockEmitter::add_exit_flags(uint8_t flags) {
    bytes({0x41, 0x83, 0xCA, flags});
}

void X86_64BlockEmitter::store_core_byte(
        int32_t displacement,
        uint8_t value) {
    bytes({0x41, 0xC6, 0x84, 0x24});
    i32(displacement);
    byte(value);
}

void X86_64BlockEmitter::store_core_qword_zero(
        int32_t displacement) {
    bytes({0x49, 0xC7, 0x84, 0x24});
    i32(displacement);
    u32(0);
}

void X86_64BlockEmitter::set_core_byte(
        uint8_t condition_opcode,
        int32_t displacement) {
    bytes({0x41, 0x0F, condition_opcode, 0x84, 0x24});
    i32(displacement);
}

void X86_64BlockEmitter::bytes(
        std::initializer_list<uint8_t> values) {
    code_.insert(code_.end(), values.begin(), values.end());
}

const std::vector<uint8_t>& X86_64BlockEmitter::code() const noexcept {
    return code_;
}
