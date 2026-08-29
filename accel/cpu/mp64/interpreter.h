#pragma once

#include <cstdint>
#include <stdexcept>

#include "decode.h"
#include "semantics.h"

namespace mp64::cpu {

#if defined(__GNUC__) || defined(__clang__)
#define MP64_INTERPRETER_ALWAYS_INLINE inline __attribute__((always_inline))
#else
#define MP64_INTERPRETER_ALWAYS_INLINE inline
#endif

struct DecodedCallAcceleration {
    bool handled = false;
    int extra_cycles = 0;
};

// Execute one fully decoded MP64 instruction. State owns the architectural
// register and flag fields used by semantics.h. Operations binds the machine
// boundary at compile time and supplies accelerate_call(), read64(),
// write64(), read8(), and write8(). This keeps instruction meaning portable
// without inserting a virtual or function-pointer dispatch into scalar memory
// and stack traffic.
template <typename State, typename Operations>
MP64_INTERPRETER_ALWAYS_INLINE int execute_decoded_instruction(
        State& state,
        Operations& operations,
        const DecodedInstruction& decoded) {
    int cycles = decoded.cycle_cost;
    switch (decoded.operation) {
        case DecodedOperation::NOP:
            break;
        case DecodedOperation::CALL_LONG: {
            // Resolve the target before stack mutation. This is observable
            // when the encoded target aliases the program or stack selector.
            const uint64_t target = state.regs[decoded.rs];
            const DecodedCallAcceleration acceleration =
                operations.accelerate_call(target);
            if (acceleration.handled) {
                // The decoded base includes the ordinary CALL stack cycle.
                // An accepted hook performs no push and replaces that cycle
                // with its architected shortcut cost.
                return cycles - 1 + acceleration.extra_cycles;
            }
            const uint64_t return_address = state.regs[state.psel];
            state.regs[state.spsel] -= 8;
            const uint64_t stack_address =
                state.regs[state.spsel];
            operations.write64(stack_address, return_address);
            state.regs[state.psel] = target;
            break;
        }
        case DecodedOperation::RETURN_LONG: {
            const uint64_t stack_address =
                state.regs[state.spsel];
            const uint64_t target = operations.read64(stack_address);
            state.regs[state.spsel] += 8;
            state.regs[state.psel] = target;
            break;
        }
        case DecodedOperation::INCREMENT:
            state.regs[decoded.rd]++;
            break;
        case DecodedOperation::DECREMENT:
            state.regs[decoded.rd]--;
            break;
        case DecodedOperation::BRANCH_SHORT:
            if (eval_cond(state, decoded.subop())) {
                state.regs[state.psel] +=
                    s64(sign_extend(decoded.immediate, 8));
                cycles += decoded.conditional_taken_cycle_cost();
            }
            break;
        case DecodedOperation::BRANCH_LONG:
            if (eval_cond(state, decoded.subop())) {
                state.regs[state.psel] +=
                    s64(sign_extend(decoded.immediate, 16));
                cycles += decoded.conditional_taken_cycle_cost();
            }
            break;
        case DecodedOperation::LOAD_NATURAL: {
            const uint64_t address = state.regs[decoded.rs];
            state.regs[decoded.rd] = operations.read64(address);
            break;
        }
        case DecodedOperation::STORE_NATURAL: {
            const uint64_t address = state.regs[decoded.rd];
            const uint64_t value = state.regs[decoded.rs];
            operations.write64(address, value);
            break;
        }
        case DecodedOperation::LOAD_BYTE: {
            const uint64_t address = state.regs[decoded.rs];
            state.regs[decoded.rd] = operations.read8(address);
            break;
        }
        case DecodedOperation::STORE_BYTE: {
            const uint64_t address = state.regs[decoded.rd];
            const uint8_t value = static_cast<uint8_t>(
                state.regs[decoded.rs]);
            operations.write8(address, value);
            break;
        }
        case DecodedOperation::LOAD_IMMEDIATE:
        case DecodedOperation::LOAD_HIGH_IMMEDIATE:
        case DecodedOperation::ADD_IMMEDIATE:
        case DecodedOperation::AND_IMMEDIATE:
        case DecodedOperation::OR_IMMEDIATE:
        case DecodedOperation::XOR_IMMEDIATE:
        case DecodedOperation::COMPARE_IMMEDIATE:
        case DecodedOperation::SUBTRACT_IMMEDIATE:
        case DecodedOperation::SHIFT_LEFT_IMMEDIATE:
        case DecodedOperation::SHIFT_RIGHT_LOGICAL_IMMEDIATE:
        case DecodedOperation::SHIFT_RIGHT_ARITHMETIC_IMMEDIATE:
        case DecodedOperation::ROTATE_LEFT_IMMEDIATE:
            execute_register_immediate(
                state,
                decoded.subop(),
                decoded.rd,
                decoded.immediate);
            break;
        case DecodedOperation::ADD:
        case DecodedOperation::ADD_WITH_CARRY:
        case DecodedOperation::SUBTRACT:
        case DecodedOperation::SUBTRACT_WITH_BORROW:
        case DecodedOperation::BITWISE_AND:
        case DecodedOperation::BITWISE_OR:
        case DecodedOperation::BITWISE_XOR:
        case DecodedOperation::COMPARE:
        case DecodedOperation::MOVE:
        case DecodedOperation::BITWISE_NOT:
        case DecodedOperation::NEGATE:
        case DecodedOperation::SHIFT_LEFT:
        case DecodedOperation::SHIFT_RIGHT_LOGICAL:
        case DecodedOperation::SHIFT_RIGHT_ARITHMETIC:
        case DecodedOperation::ROTATE_LEFT:
        case DecodedOperation::ROTATE_RIGHT:
            execute_register_alu(
                state,
                decoded.subop(),
                decoded.rd,
                decoded.rs);
            break;
        case DecodedOperation::UNSIGNED_MULTIPLY_LOW: {
            const uint64_t first = state.regs[decoded.rd];
            const uint64_t second = state.regs[decoded.rs];
            const uint64_t result = first * second;
            state.regs[decoded.rd] = result;
            state.flag_z = result == 0 ? 1 : 0;
            state.flag_n = static_cast<uint8_t>(result >> 63);
            break;
        }
        case DecodedOperation::SELECT_PROGRAM_COUNTER:
            if (state.priv_level != 0)
                throw std::runtime_error("TRAP:PRIV_FAULT");
            state.psel = decoded.rd;
            break;
        case DecodedOperation::INVALID:
        default:
            throw std::logic_error(
                "shared MP64 decoder produced an invalid operation");
    }
    return cycles;
}

#undef MP64_INTERPRETER_ALWAYS_INLINE

}  // namespace mp64::cpu
