#pragma once

#include <cstdint>
#include <limits>
#include <stdexcept>

namespace mp64::cpu {

enum ConditionCode : uint8_t {
    CC_AL = 0,
    CC_EQ,
    CC_NE,
    CC_CS,
    CC_CC,
    CC_MI,
    CC_PL,
    CC_VS,
    CC_VC,
    CC_GT,
    CC_LE,
    CC_BQ,
    CC_BNQ,
    CC_SAT,
    CC_EF,
    CC_NV,
};

inline int64_t s64(uint64_t value) noexcept {
    return static_cast<int64_t>(value);
}

inline uint64_t sign_extend(uint64_t value, int bits) noexcept {
    const uint64_t mask = (uint64_t{1} << bits) - 1;
    value &= mask;
    if (value & (uint64_t{1} << (bits - 1)))
        value |= ~mask;
    return value;
}

inline uint8_t parity8(uint64_t value) noexcept {
    uint8_t byte = static_cast<uint8_t>(value);
    byte ^= byte >> 4;
    byte ^= byte >> 2;
    byte ^= byte >> 1;
    return (byte & 1) ^ 1;
}

// Prefix values 1-5 are REX; 0 is EXT.IMM64, 6 is SKIP, and -1 is none.
inline int rex_s(int modifier) noexcept {
    return modifier >= 1 && modifier <= 5 ? modifier & 1 : 0;
}

inline int rex_d(int modifier) noexcept {
    return modifier >= 1 && modifier <= 5 ? (modifier >> 1) & 1 : 0;
}

inline int rex_n(int modifier) noexcept {
    return modifier >= 1 && modifier <= 5 ? (modifier >> 2) & 1 : 0;
}

template <typename State>
inline uint8_t flags_pack(const State& state) noexcept {
    return static_cast<uint8_t>(
        state.flag_z |
        (state.flag_c << 1) |
        (state.flag_n << 2) |
        (state.flag_v << 3) |
        (state.flag_p << 4) |
        (state.flag_g << 5) |
        (state.flag_i << 6) |
        (state.flag_s << 7));
}

template <typename State>
inline void flags_unpack(State& state, uint8_t value) noexcept {
    state.flag_z = (value >> 0) & 1;
    state.flag_c = (value >> 1) & 1;
    state.flag_n = (value >> 2) & 1;
    state.flag_v = (value >> 3) & 1;
    state.flag_p = (value >> 4) & 1;
    state.flag_g = (value >> 5) & 1;
    state.flag_i = (value >> 6) & 1;
    state.flag_s = (value >> 7) & 1;
}

template <typename State>
inline bool eval_cond(const State& state, int condition) noexcept {
    switch (condition) {
        case CC_AL: return true;
        case CC_EQ: return state.flag_z == 1;
        case CC_NE: return state.flag_z == 0;
        case CC_CS: return state.flag_c == 1;
        case CC_CC: return state.flag_c == 0;
        case CC_MI: return state.flag_n == 1;
        case CC_PL: return state.flag_n == 0;
        case CC_VS: return state.flag_v == 1;
        case CC_VC: return state.flag_v == 0;
        case CC_GT: return state.flag_g == 1;
        case CC_LE: return state.flag_g == 0;
        case CC_BQ: return state.q_out == 1;
        case CC_BNQ: return state.q_out == 0;
        case CC_SAT: return state.flag_s == 1;
        case CC_EF: return state.ef_flags != 0;
        case CC_NV: return false;
        default: return false;
    }
}

template <typename State>
inline void update_flags_arith(
        State& state,
        uint64_t first,
        uint64_t second,
        uint64_t result,
        bool subtract) noexcept {
    state.flag_z = result == 0;
    state.flag_n = (result >> 63) & 1;
    state.flag_p = parity8(result);
    if (subtract) {
        state.flag_c = first >= second;
    } else {
        const __uint128_t wide =
            static_cast<__uint128_t>(first) +
            static_cast<__uint128_t>(second);
        state.flag_c =
            wide > std::numeric_limits<uint64_t>::max();
    }
    const int64_t signed_first = s64(first);
    const int64_t signed_second = s64(second);
    const int64_t signed_result = s64(result);
    if (subtract) {
        state.flag_v =
            (signed_first >= 0 && signed_second < 0 && signed_result < 0) ||
            (signed_first < 0 && signed_second >= 0 && signed_result >= 0);
    } else {
        state.flag_v =
            (signed_first >= 0 && signed_second >= 0 && signed_result < 0) ||
            (signed_first < 0 && signed_second < 0 && signed_result >= 0);
    }
}

template <typename State>
inline void update_flags_logic(State& state, uint64_t result) noexcept {
    state.flag_z = result == 0;
    state.flag_n = (result >> 63) & 1;
    state.flag_p = parity8(result);
    state.flag_c = 0;
    state.flag_v = 0;
}

template <typename State>
inline void update_flags_cmp(
        State& state,
        uint64_t first,
        uint64_t second,
        uint64_t result) noexcept {
    update_flags_arith(state, first, second, result, true);
    state.flag_g = first > second;
}

template <typename State>
inline void execute_register_immediate(
        State& state,
        uint8_t subop,
        uint8_t reg,
        uint64_t immediate) {
    switch (subop) {
        case 0x0:  // LDI / EXT.IMM64 LDI
            state.regs[reg] = immediate;
            return;
        case 0x1:  // LHI
            state.regs[reg] =
                (state.regs[reg] & 0x0000FFFFFFFFFFFFULL) |
                ((immediate & 0xFFFF) << 48);
            return;
        case 0x2: {  // ADDI
            const uint64_t operand = sign_extend(immediate, 8);
            const uint64_t first = state.regs[reg];
            const uint64_t result = first + s64(operand);
            update_flags_arith(state, first, operand, result, false);
            state.regs[reg] = result;
            return;
        }
        case 0x3:  // ANDI
            state.regs[reg] &= immediate & 0xFF;
            update_flags_logic(state, state.regs[reg]);
            return;
        case 0x4:  // ORI
            state.regs[reg] |= immediate & 0xFF;
            update_flags_logic(state, state.regs[reg]);
            return;
        case 0x5:  // XORI
            state.regs[reg] ^= immediate & 0xFF;
            update_flags_logic(state, state.regs[reg]);
            return;
        case 0x6: {  // CMPI
            const uint64_t operand = sign_extend(immediate, 8);
            const uint64_t first = state.regs[reg];
            const uint64_t result = first - s64(operand);
            update_flags_cmp(state, first, operand, result);
            return;
        }
        case 0x7: {  // SUBI
            const uint64_t operand = sign_extend(immediate, 8);
            const uint64_t first = state.regs[reg];
            const uint64_t result = first - s64(operand);
            update_flags_arith(state, first, operand, result, true);
            state.regs[reg] = result;
            return;
        }
        case 0x8:  // LSLI
            state.regs[reg] <<= immediate & 0xF;
            return;
        case 0x9:  // LSRI
            state.regs[reg] >>= immediate & 0xF;
            return;
        case 0xA:  // ASRI
            state.regs[reg] = static_cast<uint64_t>(
                s64(state.regs[reg]) >> (immediate & 0xF));
            return;
        case 0xB: {  // ROLI
            const int shift = static_cast<int>(immediate & 0xF);
            if (shift != 0) {
                const uint64_t value = state.regs[reg];
                state.regs[reg] =
                    (value << shift) |
                    (value >> (64 - shift));
            }
            return;
        }
        default:
            throw std::logic_error(
                "decoded immediate operation is not register-private");
    }
}

template <typename State>
inline void execute_register_alu(
        State& state,
        uint8_t subop,
        uint8_t destination,
        uint8_t source) {
    const uint64_t first = state.regs[destination];
    const uint64_t second = state.regs[source];
    switch (subop) {
        case 0x0: {  // ADD
            const uint64_t result = first + second;
            update_flags_arith(state, first, second, result, false);
            state.regs[destination] = result;
            return;
        }
        case 0x1: {  // ADC
            const uint64_t operand = second + state.flag_c;
            const uint64_t result = first + operand;
            update_flags_arith(state, first, operand, result, false);
            state.regs[destination] = result;
            return;
        }
        case 0x2: {  // SUB
            const uint64_t result = first - second;
            update_flags_arith(state, first, second, result, true);
            state.regs[destination] = result;
            return;
        }
        case 0x3: {  // SBB
            const uint64_t operand = second + (1 - state.flag_c);
            const uint64_t result = first - operand;
            update_flags_arith(state, first, operand, result, true);
            state.regs[destination] = result;
            return;
        }
        case 0x4: {  // AND
            const uint64_t result = first & second;
            update_flags_logic(state, result);
            state.regs[destination] = result;
            return;
        }
        case 0x5: {  // OR
            const uint64_t result = first | second;
            update_flags_logic(state, result);
            state.regs[destination] = result;
            return;
        }
        case 0x6: {  // XOR
            const uint64_t result = first ^ second;
            update_flags_logic(state, result);
            state.regs[destination] = result;
            return;
        }
        case 0x7:  // CMP
            update_flags_cmp(state, first, second, first - second);
            return;
        case 0x8:  // MOV
            state.regs[destination] = second;
            return;
        case 0x9:  // NOT
            state.regs[destination] = ~second;
            update_flags_logic(state, state.regs[destination]);
            return;
        case 0xA: {  // NEG
            const uint64_t result = -second;
            update_flags_arith(state, 0, second, result, true);
            state.regs[destination] = result;
            return;
        }
        case 0xB: {  // SHL
            const int shift = second & 63;
            const uint64_t out_bit =
                shift ? (first >> (64 - shift)) & 1 : 0;
            const uint64_t result = first << shift;
            state.flag_z = result == 0;
            state.flag_c = out_bit;
            state.flag_n = (result >> 63) & 1;
            state.flag_p = parity8(result);
            state.regs[destination] = result;
            return;
        }
        case 0xC: {  // SHR
            const int shift = second & 63;
            const uint64_t out_bit =
                shift ? (first >> (shift - 1)) & 1 : 0;
            const uint64_t result = first >> shift;
            state.flag_z = result == 0;
            state.flag_c = out_bit;
            state.flag_n = (result >> 63) & 1;
            state.flag_p = parity8(result);
            state.regs[destination] = result;
            return;
        }
        case 0xD: {  // SAR
            const int shift = second & 63;
            const uint64_t out_bit =
                shift ? (first >> (shift - 1)) & 1 : 0;
            const uint64_t result = static_cast<uint64_t>(
                s64(first) >> shift);
            state.flag_z = result == 0;
            state.flag_c = out_bit;
            state.flag_n = (result >> 63) & 1;
            state.flag_p = parity8(result);
            state.regs[destination] = result;
            return;
        }
        case 0xE: {  // ROL
            const int shift = second & 63;
            const uint64_t result = shift
                ? (first << shift) | (first >> (64 - shift))
                : first;
            state.flag_z = result == 0;
            state.flag_n = (result >> 63) & 1;
            state.flag_p = parity8(result);
            state.regs[destination] = result;
            return;
        }
        case 0xF: {  // ROR
            const int shift = second & 63;
            const uint64_t result = shift
                ? (first >> shift) | (first << (64 - shift))
                : first;
            state.flag_z = result == 0;
            state.flag_n = (result >> 63) & 1;
            state.flag_p = parity8(result);
            state.regs[destination] = result;
            return;
        }
        default:
            throw std::logic_error("decoded ALU operation is invalid");
    }
}

}  // namespace mp64::cpu
