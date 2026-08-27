#pragma once

#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <vector>

// Host-only byte encoding. MP64 decoding, block policy, and lowering remain
// guest-owned and are intentionally not part of this interface.
class X86_64BlockEmitter {
public:
    void byte(uint8_t value);
    void u32(uint32_t value);
    void i32(int32_t value);
    void u64(uint64_t value);

    std::size_t position() const noexcept;
    std::size_t branch32(uint8_t condition);
    std::size_t jump32();
    void patch32(std::size_t displacement, std::size_t target);

    void mov_rax_from_core(int32_t displacement);
    void mov_rcx_from_core(int32_t displacement);
    void mov_r9_from_core(int32_t displacement);
    void mov_r9_immediate(uint64_t immediate);
    void mov_r8_from_pointer_table(std::size_t index);
    void mov_core_from_rax(int32_t displacement);
    void mov_core_from_rcx(int32_t displacement);
    void mov_core_from_r9(int32_t displacement);
    void add_r9_imm8(uint8_t immediate);
    void add_r9_imm32(uint32_t immediate);
    void add_core_imm8(int32_t displacement, uint8_t immediate);
    void add_core_imm32(int32_t displacement, uint32_t immediate);
    void sub_core_imm8(int32_t displacement, uint8_t immediate);
    void increment_core(int32_t displacement);
    void decrement_core(int32_t displacement);
    void add_core_r15(int32_t displacement);
    void compare_core_byte(int32_t displacement, uint8_t value);
    void add_exit_flags(uint8_t flags);
    void store_core_byte(int32_t displacement, uint8_t value);
    void store_core_qword_zero(int32_t displacement);
    void set_core_byte(uint8_t condition_opcode, int32_t displacement);

    void bytes(std::initializer_list<uint8_t> values);
    std::vector<uint8_t> release_code() noexcept;

private:
    std::vector<uint8_t> code_;
};
