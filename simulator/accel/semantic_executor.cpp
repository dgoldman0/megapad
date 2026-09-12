// Generic hosted Forth execution. This file contains no terminal/app policy.
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include <array>
#include <cstdint>
#include <limits>
#include <unordered_map>
#include <utility>
#include <vector>

namespace py = pybind11;
namespace {
using Cell = uint64_t;
constexpr Cell MASK = std::numeric_limits<Cell>::max();
constexpr Cell SIGN = Cell{1} << 63;

enum Opcode : uint32_t {
    OP_STOP,
    OP_LITERAL, OP_BRANCH, OP_BRANCH_ZERO, OP_CALL, OP_RETURN,
    OP_STORE_VALUE, OP_STRING_LITERAL,
    OP_R_PUSH, OP_R_POP, OP_R_PEEK,
    OP_PUSH_CELL, OP_FETCH_VALUE,
    OP_DUP, OP_DROP, OP_SWAP, OP_OVER, OP_NIP, OP_TUCK, OP_ROT, OP_MINUS_ROT,
    OP_TWO_DUP, OP_TWO_DROP, OP_TWO_OVER, OP_TWO_SWAP, OP_QUESTION_DUP, OP_PICK,
    OP_ADD, OP_SUBTRACT, OP_MULTIPLY, OP_DIVIDE, OP_MODULO, OP_DIVMOD,
    OP_NEGATE, OP_ABS, OP_MIN, OP_MAX, OP_ONE_PLUS, OP_ONE_MINUS,
    OP_TWO_MULTIPLY, OP_TWO_DIVIDE, OP_AND, OP_OR, OP_XOR,
    OP_LSHIFT, OP_RSHIFT, OP_INVERT,
    OP_EQUAL, OP_NOT_EQUAL, OP_ZERO_EQUAL, OP_ZERO_NOT_EQUAL,
    OP_ZERO_LESS, OP_ZERO_GREATER, OP_ULESS, OP_UGREATER,
    OP_LESS, OP_LESS_EQUAL, OP_GREATER, OP_GREATER_EQUAL, OP_WITHIN,
    OP_FETCH, OP_C_FETCH, OP_W_FETCH, OP_L_FETCH,
    OP_STORE, OP_C_STORE, OP_W_STORE, OP_L_STORE,
    OP_OFF, OP_ON, OP_PLUS_STORE, OP_COUNT,
    OP_BSWAP,
};

struct Instruction {
    uint32_t opcode;
    Cell a;
    Cell b;
};

struct Region {
    Cell base;
    Cell size;
    py::dict pages;
};

// A resolved scalar retains at most two page fragments. The page size may be
// smaller than a cell, so use eight fragments rather than assuming 4 KiB.
struct Scalar {
    std::array<uint8_t*, 8> bytes{};

    Cell read(unsigned width) const noexcept {
        Cell value = 0;
        for (unsigned i = 0; i < width; ++i) {
            if (bytes[i] != nullptr)
                value |= Cell(*bytes[i]) << (i * 8);
        }
        return value;
    }

    void write(Cell value, unsigned width) const noexcept {
        for (unsigned i = 0; i < width; ++i)
            *bytes[i] = static_cast<uint8_t>(value >> (i * 8));
    }
};

class MemoryRun {
public:
    MemoryRun(std::vector<Region>& regions, Cell page_size,
              Cell return_floor, Cell return_empty)
        : regions_(regions), page_size_(page_size),
          return_floor_(return_floor), return_empty_(return_empty),
          caches_(regions.size()) {}

    bool resolve(Cell address, unsigned width, bool write, bool ordinary,
                 Scalar& scalar) {
        if (width == 0 || width > 8 || address > MASK - (width - 1))
            return false;
        const Cell last = address + width - 1;
        if (ordinary && address < return_empty_ && last >= return_floor_)
            return false;
        for (size_t r = 0; r < regions_.size(); ++r) {
            Region& region = regions_[r];
            if (address < region.base || address - region.base >= region.size)
                continue;
            const Cell offset = address - region.base;
            if (width > region.size - offset)
                return false;
            for (unsigned i = 0; i < width; ++i) {
                const Cell cell_offset = offset + i;
                const Cell page_index = cell_offset / page_size_;
                const Cell page_offset = cell_offset % page_size_;
                uint8_t* page = nullptr;
                auto found = caches_[r].find(page_index);
                if (found != caches_[r].end()) {
                    page = found->second;
                } else {
                    py::int_ key(page_index);
                    PyObject* value = PyDict_GetItemWithError(region.pages.ptr(), key.ptr());
                    if (value == nullptr && PyErr_Occurred())
                        throw py::error_already_set();
                    if (value != nullptr) {
                        if (!PyByteArray_Check(value) ||
                            static_cast<Cell>(PyByteArray_Size(value)) != page_size_)
                            return false;
                        page = reinterpret_cast<uint8_t*>(PyByteArray_AsString(value));
                    }
                    caches_[r].emplace(page_index, page);
                }
                if (write && page == nullptr)
                    return false;
                scalar.bytes[i] = page == nullptr ? nullptr : page + page_offset;
            }
            return true;
        }
        return false;
    }

private:
    std::vector<Region>& regions_;
    Cell page_size_;
    Cell return_floor_;
    Cell return_empty_;
    std::vector<std::unordered_map<Cell, uint8_t*>> caches_;
};

struct StackState {
    Cell floor;
    Cell empty;
    Cell pointer;

    bool valid() const noexcept {
        return floor < empty && floor <= pointer && pointer <= empty &&
               ((floor | empty | pointer) & 7) == 0;
    }
    Cell depth() const noexcept { return (empty - pointer) / 8; }
};

struct ContinuationUpdate {
    Cell caller;
    Cell ip;
    Cell raw;
};

struct RunState {
    Cell xt;
    Cell ip;
    Cell steps = 0;
    StackState data;
    StackState returns;
    Cell cookie;
    std::unordered_map<Cell, ContinuationUpdate> changed;
};

static Cell flag(bool value) noexcept { return value ? MASK : 0; }
static bool less_signed(Cell left, Cell right) noexcept {
    return (left ^ SIGN) < (right ^ SIGN);
}
static Cell magnitude(Cell value) noexcept {
    return value & SIGN ? Cell{0} - value : value;
}

static unsigned cost(uint32_t opcode) noexcept {
    return opcode >= OP_LITERAL && opcode <= OP_R_PEEK ? 1 : 2;
}

// Preflight all stack operands and all output storage before the operation.
// Outputs are bottom-to-top; retained popped cells are never erased.
class StackOperation {
public:
    StackOperation(MemoryRun& memory, StackState& state)
        : memory_(memory), state_(state) {}

    bool inputs(unsigned count) {
        consumed_ = count;
        if (state_.depth() < count)
            return false;
        for (unsigned i = 0; i < count; ++i) {
            Scalar scalar;
            if (!memory_.resolve(state_.pointer + i * 8, 8, false, false, scalar))
                return false;
            values[i] = scalar.read(8);
        }
        return true;
    }

    bool outputs(unsigned count) {
        produced_ = count;
        const Cell after_pop = state_.pointer + consumed_ * 8;
        if (after_pop - state_.floor < count * 8)
            return false;
        destination_ = after_pop - count * 8;
        for (unsigned i = 0; i < count; ++i) {
            const Cell address = destination_ + (count - 1 - i) * 8;
            if (!memory_.resolve(address, 8, true, false, output_[i]))
                return false;
        }
        return true;
    }

    void commit() noexcept {
        for (unsigned i = 0; i < produced_; ++i)
            output_[i].write(result[i], 8);
        state_.pointer = destination_;
    }

    std::array<Cell, 4> values{};
    std::array<Cell, 6> result{};

private:
    MemoryRun& memory_;
    StackState& state_;
    unsigned consumed_ = 0;
    unsigned produced_ = 0;
    Cell destination_ = 0;
    std::array<Scalar, 6> output_{};
};

class NativeProgram {
public:
    NativeProgram(const py::iterable& regions, Cell page_size,
                  py::object continuation_type)
        : page_size_(page_size), continuation_type_(std::move(continuation_type)) {
        if (page_size == 0 || (page_size & (page_size - 1)) != 0)
            throw py::value_error("page size must be a positive power of two");
        for (py::handle item : regions) {
            auto entry = py::cast<py::tuple>(item);
            if (entry.size() != 3)
                throw py::value_error("region must be (base, size, pages)");
            const Cell base = entry[0].cast<Cell>();
            const Cell size = entry[1].cast<Cell>();
            if (size == 0 || base > MASK - (size - 1))
                throw py::value_error("region has an invalid ordinary span");
            for (const Region& previous : regions_) {
                if (base <= previous.base + previous.size - 1 &&
                    previous.base <= base + size - 1)
                    throw py::value_error("ordinary regions must not overlap");
            }
            regions_.push_back(Region{base, size, entry[2].cast<py::dict>()});
        }
    }

    void install(Cell xt, const py::iterable& operations) {
        if (xt == 0)
            throw py::value_error("plan execution token must be nonzero");
        std::vector<Instruction> plan;
        for (py::handle item : operations) {
            auto operation = py::cast<py::tuple>(item);
            if (operation.size() != 3)
                throw py::value_error("operation must be (opcode, a, b)");
            const auto opcode = operation[0].cast<uint32_t>();
            if (opcode > OP_BSWAP)
                throw py::value_error("unknown native semantic opcode");
            plan.push_back(Instruction{opcode, operation[1].cast<Cell>(),
                                      operation[2].cast<Cell>()});
        }
        plans_.insert_or_assign(xt, std::move(plan));
    }

    void clear() { plans_.clear(); }

    py::tuple run(Cell xt, Cell ip, const std::array<Cell, 3>& data_state,
                  const py::tuple& return_state, const py::dict& continuations,
                  Cell remaining_steps) {
        if (return_state.size() != 4)
            throw py::value_error("return state must contain four cells");
        RunState state{xt, ip, 0,
            StackState{data_state[0], data_state[1], data_state[2]},
            StackState{return_state[0].cast<Cell>(), return_state[1].cast<Cell>(),
                       return_state[2].cast<Cell>()}, 0, {}};
        // Python's continuation sequence is deliberately unbounded. Decline
        // very large sequences rather than wrap or truncate their identity.
        try {
            state.cookie = return_state[3].cast<Cell>();
        } catch (const py::cast_error&) {
            return py::make_tuple(xt, ip, 0, state.data.pointer,
                state.returns.pointer, return_state[3], py::list());
        }
        if (!state.data.valid() || !state.returns.valid() ||
            !(state.data.empty <= state.returns.floor ||
              state.returns.empty <= state.data.floor))
            return result(state);
        MemoryRun memory(regions_, page_size_, state.returns.floor,
                         state.returns.empty);
        try {
            while (state.steps < remaining_steps) {
                auto plan = plans_.find(state.xt);
                if (plan == plans_.end() || state.ip >= plan->second.size())
                    break;
                const Instruction operation = plan->second[state.ip];
                if (operation.opcode == OP_STOP ||
                    cost(operation.opcode) > remaining_steps - state.steps)
                    break;
                if (!execute(operation, memory, state, continuations))
                    break;
                state.steps += cost(operation.opcode);
            }
        } catch (const py::error_already_set&) {
            // No guest/Python callback runs here. Any page lookup failure is
            // in preflight, before the current operation has changed state.
            // Return the completed prefix so Python owns the exact fallback.
        } catch (const std::bad_alloc&) {
            // Temporary cache/continuation allocation also precedes effects.
        }
        return result(state);
    }

private:
    static py::tuple result(const RunState& state) {
        py::list updates;
        for (const auto& item : state.changed) {
            updates.append(py::make_tuple(item.first, item.second.caller,
                                          item.second.ip, item.second.raw));
        }
        return py::make_tuple(state.xt, state.ip, state.steps,
            state.data.pointer, state.returns.pointer, state.cookie, updates);
    }

    enum SlotKind { USER_CELL, CONTINUATION, PYTHON_BOUNDARY };

    SlotKind return_slot(Cell slot, Cell raw, const RunState& s,
                         const py::dict& continuations, ContinuationUpdate& value) {
        const auto changed = s.changed.find(slot);
        if (changed != s.changed.end()) {
            value = changed->second;
            if (value.caller == 0) return USER_CELL; // >R erased the old type.
            return value.raw == raw ? CONTINUATION : PYTHON_BOUNDARY;
        }
        py::int_ key(slot);
        PyObject* entry = PyDict_GetItemWithError(continuations.ptr(), key.ptr());
        if (entry == nullptr) {
            if (PyErr_Occurred()) throw py::error_already_set();
            return USER_CELL;
        }
        if (!PyTuple_CheckExact(entry) || PyTuple_GET_SIZE(entry) != 2)
            return PYTHON_BOUNDARY;
        py::handle continuation(PyTuple_GET_ITEM(entry, 0));
        if (reinterpret_cast<PyObject*>(Py_TYPE(continuation.ptr())) !=
                continuation_type_.ptr())
            return PYTHON_BOUNDARY;
        // Mismatching bytes must undergo Python's ordinary stale-type removal.
        // Root and fault returns retain their dispatcher-owned control effects.
        if (py::cast<Cell>(py::handle(PyTuple_GET_ITEM(entry, 1))) != raw ||
            continuation.attr("root").cast<bool>() ||
            continuation.attr("fault_abort").cast<bool>())
            return PYTHON_BOUNDARY;
        value = ContinuationUpdate{continuation.attr("xt").cast<Cell>(),
                                   continuation.attr("ip").cast<Cell>(), raw};
        return CONTINUATION;
    }

    bool execute(const Instruction& operation, MemoryRun& memory, RunState& s,
                 const py::dict& continuations) {
        const auto opcode = operation.opcode;
        StackOperation stack(memory, s.data);
        auto& v = stack.values;
        auto& out = stack.result;
        unsigned produced = 0;
        Scalar scalar;

        switch (opcode) {
        case OP_BRANCH:
            s.ip = operation.a;
            return true;
        case OP_BRANCH_ZERO:
            if (!stack.inputs(1) || !stack.outputs(0)) return false;
            stack.commit();
            s.ip = v[0] == 0 ? operation.a : s.ip + 1;
            return true;
        case OP_CALL: {
            auto target = plans_.find(operation.a);
            if (target == plans_.end() || target->second.empty() ||
                s.returns.pointer - s.returns.floor < 8)
                return false;
            const Cell slot = s.returns.pointer - 8;
            if (!memory.resolve(slot, 8, true, false, scalar)) return false;
            Cell cookie = s.cookie;
            Cell raw;
            do {
                if (cookie == MASK) return false;
                ++cookie;
                raw = 0xC07ECAFE00000000ULL ^ cookie;
            } while (raw == s.xt);
            // Allocate the retained side record before committing any effect.
            s.changed.insert_or_assign(slot,
                ContinuationUpdate{s.xt, s.ip + 1, raw});
            scalar.write(raw, 8);
            s.cookie = cookie;
            s.returns.pointer = slot;
            s.xt = operation.a;
            s.ip = 0;
            return true;
        }
        case OP_RETURN: {
            ContinuationUpdate continuation;
            if (s.returns.depth() == 0 ||
                !memory.resolve(s.returns.pointer, 8, false, false, scalar) ||
                return_slot(s.returns.pointer, scalar.read(8), s, continuations,
                            continuation) != CONTINUATION)
                return false;
            if (plans_.find(continuation.caller) == plans_.end()) return false;
            s.returns.pointer += 8;
            s.xt = continuation.caller;
            s.ip = continuation.ip;
            return true;
        }
        case OP_R_PUSH: {
            if (!stack.inputs(1) || !stack.outputs(0) ||
                s.returns.pointer - s.returns.floor < 8) return false;
            const Cell slot = s.returns.pointer - 8;
            if (!memory.resolve(slot, 8, true, false, scalar)) return false;
            // A user push deletes type metadata even when its value happens to
            // equal the old cookie. Retain that deletion across native exits.
            s.changed.insert_or_assign(slot, ContinuationUpdate{0, 0, 0});
            scalar.write(v[0], 8);
            s.returns.pointer = slot;
            stack.commit();
            ++s.ip;
            return true;
        }
        case OP_R_POP: case OP_R_PEEK: {
            ContinuationUpdate continuation;
            if (!stack.inputs(0) || !stack.outputs(1) || s.returns.depth() == 0 ||
                !memory.resolve(s.returns.pointer, 8, false, false, scalar) ||
                return_slot(s.returns.pointer, scalar.read(8), s, continuations,
                            continuation) != USER_CELL) return false;
            out[0] = scalar.read(8);
            if (opcode == OP_R_POP) s.returns.pointer += 8;
            stack.commit();
            ++s.ip;
            return true;
        }
        case OP_LITERAL: case OP_PUSH_CELL:
            if (!stack.inputs(0)) return false;
            out[0] = operation.a; produced = 1;
            break;
        case OP_STRING_LITERAL:
            if (!stack.inputs(0)) return false;
            out[0] = operation.a; out[1] = operation.b; produced = 2;
            break;
        case OP_FETCH_VALUE:
            if (!stack.inputs(0) ||
                !memory.resolve(operation.a, 8, false, true, scalar)) return false;
            out[0] = scalar.read(8); produced = 1;
            break;
        case OP_STORE_VALUE:
            if (!stack.inputs(1) || !stack.outputs(0) ||
                !memory.resolve(operation.a, 8, true, true, scalar)) return false;
            stack.commit();
            scalar.write(v[0], 8);
            ++s.ip;
            return true;
        case OP_DUP: case OP_QUESTION_DUP:
            if (!stack.inputs(1)) return false;
            out[0] = v[0]; out[1] = v[0];
            produced = opcode == OP_DUP || v[0] != 0 ? 2 : 1;
            break;
        case OP_DROP: case OP_TWO_DROP:
            if (!stack.inputs(opcode == OP_DROP ? 1 : 2)) return false;
            break;
        case OP_SWAP:
            if (!stack.inputs(2)) return false;
            out[0] = v[0]; out[1] = v[1]; produced = 2;
            break;
        case OP_OVER:
            if (!stack.inputs(2)) return false;
            out[0] = v[1]; out[1] = v[0]; out[2] = v[1]; produced = 3;
            break;
        case OP_NIP:
            if (!stack.inputs(2)) return false;
            out[0] = v[0]; produced = 1;
            break;
        case OP_TUCK:
            if (!stack.inputs(2)) return false;
            out[0] = v[0]; out[1] = v[1]; out[2] = v[0]; produced = 3;
            break;
        case OP_ROT: case OP_MINUS_ROT:
            if (!stack.inputs(3)) return false;
            out[0] = opcode == OP_ROT ? v[1] : v[0];
            out[1] = opcode == OP_ROT ? v[0] : v[2];
            out[2] = opcode == OP_ROT ? v[2] : v[1]; produced = 3;
            break;
        case OP_TWO_DUP:
            if (!stack.inputs(2)) return false;
            out[0] = v[1]; out[1] = v[0]; out[2] = v[1]; out[3] = v[0];
            produced = 4;
            break;
        case OP_TWO_OVER:
            if (!stack.inputs(4)) return false;
            out[0] = v[3]; out[1] = v[2]; out[2] = v[1]; out[3] = v[0];
            out[4] = v[3]; out[5] = v[2]; produced = 6;
            break;
        case OP_TWO_SWAP:
            if (!stack.inputs(4)) return false;
            out[0] = v[1]; out[1] = v[0]; out[2] = v[3]; out[3] = v[2];
            produced = 4;
            break;
        case OP_PICK:
            if (!stack.inputs(1) || s.data.depth() < 2 ||
                v[0] > s.data.depth() - 2 ||
                !memory.resolve(s.data.pointer + (v[0] + 1) * 8,
                                8, false, false, scalar)) return false;
            out[0] = scalar.read(8); produced = 1;
            break;
        case OP_NEGATE: case OP_ABS: case OP_ONE_PLUS: case OP_ONE_MINUS:
        case OP_TWO_MULTIPLY: case OP_TWO_DIVIDE: case OP_INVERT:
        case OP_ZERO_EQUAL: case OP_ZERO_NOT_EQUAL: case OP_ZERO_LESS:
        case OP_ZERO_GREATER: case OP_BSWAP:
            if (!stack.inputs(1)) return false;
            produced = 1;
            switch (opcode) {
            case OP_NEGATE: out[0] = Cell{0} - v[0]; break;
            case OP_ABS: out[0] = magnitude(v[0]); break;
            case OP_ONE_PLUS: out[0] = v[0] + 1; break;
            case OP_ONE_MINUS: out[0] = v[0] - 1; break;
            case OP_TWO_MULTIPLY: out[0] = v[0] << 1; break;
            case OP_TWO_DIVIDE: out[0] = (v[0] >> 1) | (v[0] & SIGN); break;
            case OP_INVERT: out[0] = ~v[0]; break;
            case OP_ZERO_EQUAL: out[0] = flag(v[0] == 0); break;
            case OP_ZERO_NOT_EQUAL: out[0] = flag(v[0] != 0); break;
            case OP_ZERO_LESS: out[0] = flag((v[0] & SIGN) != 0); break;
            case OP_ZERO_GREATER: out[0] = flag(v[0] != 0 && !(v[0] & SIGN)); break;
            case OP_BSWAP:
                for (unsigned i = 0; i < 8; ++i)
                    out[0] |= ((v[0] >> (i * 8)) & 255) << ((7 - i) * 8);
                break;
            default: return false;
            }
            break;
        case OP_ADD: case OP_SUBTRACT: case OP_MULTIPLY:
        case OP_DIVIDE: case OP_MODULO: case OP_DIVMOD:
        case OP_MIN: case OP_MAX: case OP_AND: case OP_OR: case OP_XOR:
        case OP_LSHIFT: case OP_RSHIFT: case OP_EQUAL: case OP_NOT_EQUAL:
        case OP_ULESS: case OP_UGREATER: case OP_LESS: case OP_LESS_EQUAL:
        case OP_GREATER: case OP_GREATER_EQUAL:
            if (!stack.inputs(2)) return false;
            produced = 1;
            switch (opcode) {
            case OP_ADD: out[0] = v[1] + v[0]; break;
            case OP_SUBTRACT: out[0] = v[1] - v[0]; break;
            case OP_MULTIPLY: out[0] = v[1] * v[0]; break;
            case OP_DIVIDE: case OP_MODULO: case OP_DIVMOD: {
                if (v[0] == 0 || (opcode != OP_MODULO && v[1] == SIGN && v[0] == MASK))
                    return false;
                Cell quotient = magnitude(v[1]) / magnitude(v[0]);
                if ((v[1] ^ v[0]) & SIGN) quotient = Cell{0} - quotient;
                const Cell remainder = v[1] - quotient * v[0];
                out[0] = opcode == OP_DIVIDE ? quotient : remainder;
                if (opcode == OP_DIVMOD) { out[1] = quotient; produced = 2; }
                break;
            }
            case OP_MIN: out[0] = less_signed(v[0], v[1]) ? v[0] : v[1]; break;
            case OP_MAX: out[0] = less_signed(v[1], v[0]) ? v[0] : v[1]; break;
            case OP_AND: out[0] = v[1] & v[0]; break;
            case OP_OR: out[0] = v[1] | v[0]; break;
            case OP_XOR: out[0] = v[1] ^ v[0]; break;
            case OP_LSHIFT: out[0] = v[1] << (v[0] & 63); break;
            case OP_RSHIFT: out[0] = v[1] >> (v[0] & 63); break;
            case OP_EQUAL: out[0] = flag(v[1] == v[0]); break;
            case OP_NOT_EQUAL: out[0] = flag(v[1] != v[0]); break;
            case OP_ULESS: out[0] = flag(v[1] < v[0]); break;
            case OP_UGREATER: out[0] = flag(v[1] > v[0]); break;
            case OP_LESS: out[0] = flag(less_signed(v[1], v[0])); break;
            case OP_LESS_EQUAL: out[0] = flag(!less_signed(v[0], v[1])); break;
            case OP_GREATER: out[0] = flag(less_signed(v[0], v[1])); break;
            case OP_GREATER_EQUAL: out[0] = flag(!less_signed(v[1], v[0])); break;
            default: return false;
            }
            break;
        case OP_WITHIN:
            if (!stack.inputs(3)) return false;
            out[0] = flag(v[2] - v[1] < v[0] - v[1]); produced = 1;
            break;
        case OP_FETCH: case OP_C_FETCH: case OP_W_FETCH: case OP_L_FETCH: {
            const unsigned width = opcode == OP_FETCH ? 8 :
                opcode == OP_C_FETCH ? 1 : opcode == OP_W_FETCH ? 2 : 4;
            if (!stack.inputs(1) || !memory.resolve(v[0], width, false, true, scalar))
                return false;
            out[0] = scalar.read(width); produced = 1;
            break;
        }
        case OP_COUNT:
            if (!stack.inputs(1) || !memory.resolve(v[0], 1, false, true, scalar))
                return false;
            out[0] = v[0] + 1; out[1] = scalar.read(1); produced = 2;
            break;
        case OP_STORE: case OP_C_STORE: case OP_W_STORE: case OP_L_STORE:
        case OP_OFF: case OP_ON: case OP_PLUS_STORE: {
            const bool unary = opcode == OP_OFF || opcode == OP_ON;
            const unsigned width = opcode == OP_C_STORE ? 1 :
                opcode == OP_W_STORE ? 2 : opcode == OP_L_STORE ? 4 : 8;
            if (!stack.inputs(unary ? 1 : 2) || !stack.outputs(0) ||
                !memory.resolve(v[0], width, true, true, scalar)) return false;
            const Cell value = opcode == OP_OFF ? 0 : opcode == OP_ON ? MASK :
                opcode == OP_PLUS_STORE ? scalar.read(8) + v[1] : v[1];
            stack.commit();
            scalar.write(value, width);
            ++s.ip;
            return true;
        }
        default:
            return false;
        }
        if (!stack.outputs(produced)) return false;
        stack.commit();
        ++s.ip;
        return true;
    }

    Cell page_size_;
    py::object continuation_type_;
    std::vector<Region> regions_;
    std::unordered_map<Cell, std::vector<Instruction>> plans_;
};
}  // namespace

PYBIND11_MODULE(_megaforth_native, module) {
    module.doc() = "Native execution of generic hosted Forth semantic plans";
    py::class_<NativeProgram>(module, "NativeProgram")
        .def(py::init<const py::iterable&, Cell, py::object>(), py::arg("regions"),
             py::arg("page_size"), py::arg("continuation_type"))
        .def("install", &NativeProgram::install, py::arg("xt"), py::arg("operations"))
        .def("clear", &NativeProgram::clear)
        .def("run", &NativeProgram::run, py::arg("xt"), py::arg("ip"),
             py::arg("data_state"), py::arg("return_state"),
             py::arg("continuations"), py::arg("remaining_steps"));
#define EXPORT_OPCODE(name) module.attr(#name) = py::int_(static_cast<uint32_t>(name))
    EXPORT_OPCODE(OP_STOP);
    EXPORT_OPCODE(OP_LITERAL); EXPORT_OPCODE(OP_BRANCH); EXPORT_OPCODE(OP_BRANCH_ZERO);
    EXPORT_OPCODE(OP_CALL); EXPORT_OPCODE(OP_RETURN); EXPORT_OPCODE(OP_STORE_VALUE);
    EXPORT_OPCODE(OP_STRING_LITERAL); EXPORT_OPCODE(OP_PUSH_CELL); EXPORT_OPCODE(OP_FETCH_VALUE);
    EXPORT_OPCODE(OP_R_PUSH); EXPORT_OPCODE(OP_R_POP); EXPORT_OPCODE(OP_R_PEEK);
    py::dict primitives;
#define PRIMITIVE(word, name) EXPORT_OPCODE(name); primitives[py::bytes(word)] = py::int_(static_cast<uint32_t>(name))
    PRIMITIVE("DUP", OP_DUP); PRIMITIVE("DROP", OP_DROP); PRIMITIVE("SWAP", OP_SWAP);
    PRIMITIVE("OVER", OP_OVER); PRIMITIVE("NIP", OP_NIP); PRIMITIVE("TUCK", OP_TUCK);
    PRIMITIVE("ROT", OP_ROT); PRIMITIVE("-ROT", OP_MINUS_ROT);
    PRIMITIVE("2DUP", OP_TWO_DUP); PRIMITIVE("2DROP", OP_TWO_DROP);
    PRIMITIVE("2OVER", OP_TWO_OVER); PRIMITIVE("2SWAP", OP_TWO_SWAP);
    PRIMITIVE("?DUP", OP_QUESTION_DUP); PRIMITIVE("PICK", OP_PICK);
    PRIMITIVE("+", OP_ADD); PRIMITIVE("-", OP_SUBTRACT); PRIMITIVE("*", OP_MULTIPLY);
    PRIMITIVE("/", OP_DIVIDE); PRIMITIVE("MOD", OP_MODULO); PRIMITIVE("/MOD", OP_DIVMOD);
    PRIMITIVE("NEGATE", OP_NEGATE); PRIMITIVE("ABS", OP_ABS);
    PRIMITIVE("MIN", OP_MIN); PRIMITIVE("MAX", OP_MAX);
    PRIMITIVE("1+", OP_ONE_PLUS); PRIMITIVE("1-", OP_ONE_MINUS);
    PRIMITIVE("2*", OP_TWO_MULTIPLY); PRIMITIVE("2/", OP_TWO_DIVIDE);
    PRIMITIVE("AND", OP_AND); PRIMITIVE("OR", OP_OR); PRIMITIVE("XOR", OP_XOR);
    PRIMITIVE("LSHIFT", OP_LSHIFT); PRIMITIVE("RSHIFT", OP_RSHIFT); PRIMITIVE("INVERT", OP_INVERT);
    PRIMITIVE("=", OP_EQUAL); PRIMITIVE("<>", OP_NOT_EQUAL);
    PRIMITIVE("0=", OP_ZERO_EQUAL); PRIMITIVE("0<>", OP_ZERO_NOT_EQUAL);
    PRIMITIVE("0<", OP_ZERO_LESS); PRIMITIVE("0>", OP_ZERO_GREATER);
    PRIMITIVE("U<", OP_ULESS); PRIMITIVE("U>", OP_UGREATER);
    PRIMITIVE("<", OP_LESS); PRIMITIVE("<=", OP_LESS_EQUAL);
    PRIMITIVE(">", OP_GREATER); PRIMITIVE(">=", OP_GREATER_EQUAL);
    PRIMITIVE("WITHIN", OP_WITHIN);
    PRIMITIVE("@", OP_FETCH); PRIMITIVE("C@", OP_C_FETCH);
    PRIMITIVE("W@", OP_W_FETCH); PRIMITIVE("L@", OP_L_FETCH);
    PRIMITIVE("!", OP_STORE); PRIMITIVE("C!", OP_C_STORE);
    PRIMITIVE("W!", OP_W_STORE); PRIMITIVE("L!", OP_L_STORE);
    PRIMITIVE("OFF", OP_OFF); PRIMITIVE("ON", OP_ON); PRIMITIVE("+!", OP_PLUS_STORE);
    PRIMITIVE("COUNT", OP_COUNT); PRIMITIVE("BSWAP", OP_BSWAP);
    module.attr("PRIMITIVE_OPCODES") = primitives;
#undef PRIMITIVE
#undef EXPORT_OPCODE
}
