#pragma once

#include "semantics.h"

namespace mp64::cpu {
namespace {

template <typename Reader>
class DecodeCursor {
public:
    explicit DecodeCursor(Reader& reader) noexcept
        : reader_(reader) {}

    bool read(uint8_t& value) {
        if (
            !reader_.read(value)
        ) {
            return false;
        }
        ++consumed_;
        return true;
    }

    void observe_prefix(uint8_t modifier) const {
        reader_.observe_prefix(modifier);
    }

    uint8_t consumed() const noexcept {
        return consumed_;
    }

private:
    Reader& reader_;
    uint8_t consumed_ = 0;
};

bool is_extension_engine(uint8_t subop) noexcept {
    return subop == 0x9 || subop == 0xA || subop == 0xB;
}

void add_trait(
        DecodedInstruction& instruction,
        DecodedInstructionTrait trait) noexcept {
    instruction.traits = static_cast<uint8_t>(
        instruction.traits | static_cast<uint8_t>(trait));
}

DecodedOperation immediate_operation(uint8_t subop) noexcept {
    switch (subop) {
        case 0x0: return DecodedOperation::LOAD_IMMEDIATE;
        case 0x1: return DecodedOperation::LOAD_HIGH_IMMEDIATE;
        case 0x2: return DecodedOperation::ADD_IMMEDIATE;
        case 0x3: return DecodedOperation::AND_IMMEDIATE;
        case 0x4: return DecodedOperation::OR_IMMEDIATE;
        case 0x5: return DecodedOperation::XOR_IMMEDIATE;
        case 0x6: return DecodedOperation::COMPARE_IMMEDIATE;
        case 0x7: return DecodedOperation::SUBTRACT_IMMEDIATE;
        case 0x8: return DecodedOperation::SHIFT_LEFT_IMMEDIATE;
        case 0x9: return DecodedOperation::SHIFT_RIGHT_LOGICAL_IMMEDIATE;
        case 0xA: return DecodedOperation::SHIFT_RIGHT_ARITHMETIC_IMMEDIATE;
        case 0xB: return DecodedOperation::ROTATE_LEFT_IMMEDIATE;
        default: return DecodedOperation::INVALID;
    }
}

DecodedOperation alu_operation(uint8_t subop) noexcept {
    switch (subop) {
        case 0x0: return DecodedOperation::ADD;
        case 0x1: return DecodedOperation::ADD_WITH_CARRY;
        case 0x2: return DecodedOperation::SUBTRACT;
        case 0x3: return DecodedOperation::SUBTRACT_WITH_BORROW;
        case 0x4: return DecodedOperation::BITWISE_AND;
        case 0x5: return DecodedOperation::BITWISE_OR;
        case 0x6: return DecodedOperation::BITWISE_XOR;
        case 0x7: return DecodedOperation::COMPARE;
        case 0x8: return DecodedOperation::MOVE;
        case 0x9: return DecodedOperation::BITWISE_NOT;
        case 0xA: return DecodedOperation::NEGATE;
        case 0xB: return DecodedOperation::SHIFT_LEFT;
        case 0xC: return DecodedOperation::SHIFT_RIGHT_LOGICAL;
        case 0xD: return DecodedOperation::SHIFT_RIGHT_ARITHMETIC;
        case 0xE: return DecodedOperation::ROTATE_LEFT;
        case 0xF: return DecodedOperation::ROTATE_RIGHT;
        default: return DecodedOperation::INVALID;
    }
}

}  // namespace

template <typename Reader>
DecodeResult decode_instruction(
        Reader& reader,
        int initial_modifier) {
    DecodeResult result;
    result.modifier = initial_modifier;
    DecodeCursor<Reader> cursor(reader);

    const auto unavailable = [&]() {
        result.status = DecodeStatus::UNAVAILABLE;
        result.bytes_consumed = cursor.consumed();
        return result;
    };
    const auto deferred = [&]() {
        result.status = DecodeStatus::DEFERRED;
        result.bytes_consumed = cursor.consumed();
        return result;
    };

    uint8_t opcode = 0;
    if (!cursor.read(opcode))
        return unavailable();

    result.family = static_cast<uint8_t>(opcode >> 4);
    result.subop = static_cast<uint8_t>(opcode & 0x0F);

    bool parsed_prefix = false;
    if (result.family == 0xF) {
        // F9-FB are extension-engine headers, not modifiers. Their engines
        // own all tail decoding.
        if (is_extension_engine(result.subop))
            return deferred();

        parsed_prefix = true;
        result.prefix_size = 1;
        result.modifier = result.subop;
        cursor.observe_prefix(result.subop);

        if (!cursor.read(opcode))
            return unavailable();
        result.family = static_cast<uint8_t>(opcode >> 4);
        result.subop = static_cast<uint8_t>(opcode & 0x0F);

        if (result.family == 0xF) {
            // A modifier may prefix an extension engine, which will consume
            // its own tail. Any other second F-family byte is illegal.
            if (is_extension_engine(result.subop))
                return deferred();
            result.status = DecodeStatus::ILLEGAL_DOUBLE_PREFIX;
            result.bytes_consumed = cursor.consumed();
            return result;
        }
    }

    DecodedInstruction decoded;
    decoded.opcode = opcode;
    decoded.cycle_cost = static_cast<uint8_t>(1 + parsed_prefix);
    if (parsed_prefix)
        add_trait(decoded, PREFIXED_ENCODING);

    switch (result.family) {
        case 0x0: {
            switch (result.subop) {
                case 0x1:  // NOP
                    decoded.operation = DecodedOperation::NOP;
                    break;
                case 0xD: {  // CALL.L
                    uint8_t operand = 0;
                    if (!cursor.read(operand))
                        return unavailable();
                    decoded.operation = DecodedOperation::CALL_LONG;
                    decoded.rs = static_cast<uint8_t>(
                        (operand & 0x0F) |
                        (rex_s(result.modifier) << 4));
                    decoded.cycle_cost++;
                    add_trait(decoded, TERMINAL_CONTROL);
                    add_trait(decoded, MEMORY_OPERATION);
                    if ((operand & 0xF0) != 0)
                        add_trait(decoded, NONCANONICAL_ENCODING);
                    break;
                }
                case 0xE:  // RET.L
                    decoded.operation = DecodedOperation::RETURN_LONG;
                    decoded.cycle_cost++;
                    add_trait(decoded, TERMINAL_CONTROL);
                    add_trait(decoded, MEMORY_OPERATION);
                    break;
                default:
                    return deferred();
            }
            break;
        }

        case 0x1:  // INC Rn
            decoded.operation = DecodedOperation::INCREMENT;
            decoded.rd = static_cast<uint8_t>(
                result.subop | (rex_n(result.modifier) << 4));
            add_trait(decoded, WRITES_DESTINATION);
            break;

        case 0x2:  // DEC Rn
            decoded.operation = DecodedOperation::DECREMENT;
            decoded.rd = static_cast<uint8_t>(
                result.subop | (rex_n(result.modifier) << 4));
            add_trait(decoded, WRITES_DESTINATION);
            break;

        case 0x3: {  // BR / SKIP
            // F6 changes this family into SKIP. Leave its tail untouched for
            // the authoritative variable-length instruction-size path.
            if (result.modifier == 6)
                return deferred();
            uint8_t offset = 0;
            if (!cursor.read(offset))
                return unavailable();
            decoded.operation = DecodedOperation::BRANCH_SHORT;
            decoded.immediate = offset;
            add_trait(decoded, TERMINAL_CONTROL);
            if (result.subop == CC_AL) {
                decoded.cycle_cost++;
            } else {
                add_trait(decoded, CONDITIONAL_CONTROL);
            }
            break;
        }

        case 0x4: {  // LBR
            uint8_t high = 0;
            uint8_t low = 0;
            if (!cursor.read(high) || !cursor.read(low))
                return unavailable();
            decoded.operation = DecodedOperation::BRANCH_LONG;
            decoded.immediate =
                (static_cast<uint64_t>(high) << 8) | low;
            add_trait(decoded, TERMINAL_CONTROL);
            if (result.subop == CC_AL) {
                decoded.cycle_cost++;
            } else {
                add_trait(decoded, CONDITIONAL_CONTROL);
            }
            break;
        }

        case 0x5: {  // Direct scalar memory subset
            switch (result.subop) {
                case 0x0:
                    decoded.operation = DecodedOperation::LOAD_NATURAL;
                    break;
                case 0x4:
                    decoded.operation = DecodedOperation::STORE_NATURAL;
                    break;
                case 0x6:
                    decoded.operation = DecodedOperation::LOAD_BYTE;
                    break;
                case 0x7:
                    decoded.operation = DecodedOperation::STORE_BYTE;
                    break;
                default:
                    return deferred();
            }

            uint8_t operands = 0;
            if (!cursor.read(operands))
                return unavailable();
            decoded.rd = static_cast<uint8_t>(
                ((operands >> 4) & 0x0F) |
                (rex_d(result.modifier) << 4));
            decoded.rs = static_cast<uint8_t>(
                (operands & 0x0F) |
                (rex_s(result.modifier) << 4));
            add_trait(decoded, MEMORY_OPERATION);
            if (
                decoded.operation == DecodedOperation::LOAD_NATURAL ||
                decoded.operation == DecodedOperation::LOAD_BYTE
            ) {
                add_trait(decoded, DIRECT_MEMORY_READ);
                add_trait(decoded, WRITES_DESTINATION);
            } else {
                add_trait(decoded, DIRECT_MEMORY_STORE);
            }
            break;
        }

        case 0x6: {  // IMM
            if (result.subop > 0xB)
                return deferred();

            uint8_t operands = 0;
            if (!cursor.read(operands))
                return unavailable();
            decoded.operation = immediate_operation(result.subop);
            decoded.rd = static_cast<uint8_t>(
                ((operands >> 4) & 0x0F) |
                (rex_d(result.modifier) << 4));

            if (result.subop == 0x0) {
                if (result.modifier == 0) {
                    for (int index = 0; index < 8; ++index) {
                        uint8_t byte = 0;
                        if (!cursor.read(byte))
                            return unavailable();
                        decoded.immediate |=
                            static_cast<uint64_t>(byte) << (8 * index);
                    }
                } else {
                    uint8_t byte = 0;
                    if (!cursor.read(byte))
                        return unavailable();
                    decoded.immediate = byte;
                }
            } else if (result.subop == 0x1) {
                uint8_t low = 0;
                uint8_t high = 0;
                if (!cursor.read(low) || !cursor.read(high))
                    return unavailable();
                decoded.immediate =
                    low | (static_cast<uint64_t>(high) << 8);
            } else if (result.subop <= 0x7) {
                uint8_t byte = 0;
                if (!cursor.read(byte))
                    return unavailable();
                decoded.immediate = byte;
            } else {
                decoded.immediate = operands & 0x0F;
            }

            if (result.subop != 0x6)
                add_trait(decoded, WRITES_DESTINATION);
            break;
        }

        case 0x7: {  // ALU
            uint8_t operands = 0;
            if (!cursor.read(operands))
                return unavailable();
            decoded.operation = alu_operation(result.subop);
            decoded.rd = static_cast<uint8_t>(
                ((operands >> 4) & 0x0F) |
                (rex_d(result.modifier) << 4));
            decoded.rs = static_cast<uint8_t>(
                (operands & 0x0F) |
                (rex_s(result.modifier) << 4));
            if (result.subop != 0x7)
                add_trait(decoded, WRITES_DESTINATION);
            break;
        }

        case 0xA:  // SEP Rn
            decoded.operation = DecodedOperation::SELECT_PROGRAM_COUNTER;
            decoded.rd = static_cast<uint8_t>(
                result.subop | (rex_n(result.modifier) << 4));
            add_trait(decoded, TERMINAL_CONTROL);
            break;

        default:
            return deferred();
    }

    decoded.encoded_size = cursor.consumed();
    result.instruction = decoded;
    result.status = DecodeStatus::DECODED;
    result.bytes_consumed = cursor.consumed();
    return result;
}

}  // namespace mp64::cpu
