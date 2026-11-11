// Generic hosted Forth execution. This file contains no terminal/app policy.
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include "../../shared/accel/scalar_fp_bindings.h"
#include "../../shared/accel/keccak_bindings.h"
#include "../../shared/accel/tile_values_bindings.h"
#include "../../shared/accel/chiplet_bindings.h"
#include "../../shared/accel/routine_call.h"
#include "collected.h"
#include "guest_memory.h"
#include "dictionary.h"

namespace megapad::walker {
void register_bindings(pybind11::module_& module);
}

#include <algorithm>
#include <array>
#include <cstdint>
#include <cstring>
#include <deque>
#include <limits>
#include <memory>
#include <unordered_map>
#include <unordered_set>
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
    OP_DO, OP_QUESTION_DO, OP_LOOP, OP_PLUS_LOOP, OP_UNLOOP,
    OP_R_PUSH_PAIR, OP_R_POP_PAIR, OP_R_PEEK_PAIR, OP_ABORT_IF,
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
    OP_TRUE, OP_FALSE, OP_CELLS, OP_UMULTIPLY, OP_I, OP_J,
    OP_BSWAP, OP_CELL_PLUS, OP_EXECUTE, OP_COMPARE, OP_FILL,
    OP_CMOVE, OP_CMOVE_UP, OP_MOVE,
    OP_SP_FETCH, OP_RP_FETCH,
    OP_SCALAR_FP, OP_FPCSR_FETCH, OP_FPCSR_STORE,
    OP_CALL_ROUTINE,
    // Legacy tile engine words; their registers travel with each run.
    OP_TILE_MODE_SET, OP_TILE_MODE_STORE, OP_TILE_SOURCE0_STORE,
    OP_TILE_SOURCE1_STORE, OP_TILE_DESTINATION_STORE, OP_TILE_BINARY,
    // A call to a CREATE ... DOES> child: operand a is the child's body.
    OP_DOES,
};

// TADD, TSUB and TMUL, in OP_TILE_BINARY's operand a.
enum TileBinary : uint32_t { TILE_ADD, TILE_SUBTRACT, TILE_MULTIPLY };

struct Instruction;
using Plan = std::vector<Instruction>;

// Where a DOES> child continues: the defining word and the index of its
// first operation after DOES>, with that word's plan once a call has found it.
struct DoesEntry {
    Cell xt;
    Cell ip;
    Plan* plan = nullptr;
};

struct Instruction {
    uint32_t opcode;
    // The original instruction and every IR index remain available. A fused
    // successor is only used when both operations can finish without exiting.
    uint32_t fused = OP_STOP;
    Cell a;
    union {
        Cell b;
        // Calls do not use operand b. Plan objects remain at stable map-node
        // addresses across installation/rehash; clear discards every caller.
        Plan* target;
        // OP_DOES names its entry by index when installed and holds the
        // entry itself from then on. Entries keep their addresses until clear.
        DoesEntry* does;
    };
};

using megapad::guest::Region;

struct ByteChunk {
    uint8_t* bytes;
    Cell size;
};

// Common strings fit one page without allocating a chunk list. Longer spans
// retain every qualified page before any guest mutation is committed.
class ByteSpan {
public:
    void append(uint8_t* bytes, Cell size) {
        if (first_.size == 0) first_ = ByteChunk{bytes, size};
        else rest_.push_back(ByteChunk{bytes, size});
    }
    size_t chunks() const noexcept { return first_.size == 0 ? 0 : 1 + rest_.size(); }
    const ByteChunk& operator[](size_t index) const noexcept {
        return index == 0 ? first_ : rest_[index - 1];
    }
    void fill(uint8_t value) const noexcept {
        for (size_t i = 0; i < chunks(); ++i) {
            const auto& chunk = (*this)[i];
            if (chunk.bytes != nullptr)
                std::memset(chunk.bytes, value, static_cast<size_t>(chunk.size));
        }
    }
    void read_into(uint8_t* destination) const noexcept {
        for (size_t i = 0; i < chunks(); ++i) {
            const auto& chunk = (*this)[i];
            const auto size = static_cast<size_t>(chunk.size);
            if (chunk.bytes == nullptr) std::memset(destination, 0, size);
            else std::memcpy(destination, chunk.bytes, size);
            destination += size;
        }
    }
    void write_from(const uint8_t* source) const noexcept {
        for (size_t i = 0; i < chunks(); ++i) {
            const auto& chunk = (*this)[i];
            const auto size = static_cast<size_t>(chunk.size);
            std::memcpy(chunk.bytes, source, size);
            source += size;
        }
    }
    int compare(const ByteSpan& other) const noexcept {
        size_t left_index = 0, right_index = 0;
        Cell left_offset = 0, right_offset = 0;
        while (left_index < chunks()) {
            const auto& left = (*this)[left_index];
            const auto& right = other[right_index];
            const Cell size = std::min(left.size - left_offset, right.size - right_offset);
            const auto* a = left.bytes == nullptr ? nullptr : left.bytes + left_offset;
            const auto* b = right.bytes == nullptr ? nullptr : right.bytes + right_offset;
            if (a != nullptr && b != nullptr) {
                const int order = std::memcmp(a, b, static_cast<size_t>(size));
                if (order != 0) return order < 0 ? -1 : 1;
            } else if (a != b) {
                const auto* present = a == nullptr ? b : a;
                for (Cell i = 0; i < size; ++i)
                    if (present[i] != 0) return a == nullptr ? -1 : 1;
            }
            left_offset += size;
            right_offset += size;
            if (left_offset == left.size) { ++left_index; left_offset = 0; }
            if (right_offset == right.size) { ++right_index; right_offset = 0; }
        }
        return 0;
    }

private:
    ByteChunk first_{nullptr, 0};
    std::vector<ByteChunk> rest_;
};

// Ordinary scalars usually occupy one page. The scatter representation also
// supports page sizes smaller than a cell, without copying the guest bytes.
struct Scalar {
    uint8_t* contiguous;
    bool fragmented;
    std::array<uint8_t*, 8> bytes;

    Cell read(unsigned width) const noexcept {
        Cell value = 0;
        if (!fragmented) {
            if (contiguous != nullptr) {
#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
                if (width == 8) {
                    std::memcpy(&value, contiguous, sizeof(value));
                    return value;
                }
#endif
                for (unsigned i = 0; i < width; ++i)
                    value |= Cell(contiguous[i]) << (i * 8);
            }
            return value;
        }
        for (unsigned i = 0; i < width; ++i) {
            if (bytes[i] != nullptr)
                value |= Cell(*bytes[i]) << (i * 8);
        }
        return value;
    }

    void write(Cell value, unsigned width) const noexcept {
        if (!fragmented) {
#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
            if (width == 8) {
                std::memcpy(contiguous, &value, sizeof(value));
                return;
            }
#endif
            for (unsigned i = 0; i < width; ++i)
                contiguous[i] = static_cast<uint8_t>(value >> (i * 8));
            return;
        }
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
          caches_(regions.size()) {
        for (Cell size = page_size; size > 1; size >>= 1) ++page_shift_;
    }

    bool resolve(Cell address, unsigned width, bool write, bool ordinary,
                 Scalar& scalar) {
        if (width == 0 || width > 8 || address > MASK - (width - 1))
            return false;
        const Cell last = address + width - 1;
        if (ordinary && address < return_empty_ && last >= return_floor_)
            return false;
        // Each access role usually stays on one page. Keep exact qualified
        // spans only for this GIL-held run; stores still hit the shared bytes.
        HotPage& hot = ordinary ? ordinary_page_ :
            address >= return_floor_ && address < return_empty_
                ? return_page_ : data_page_;
        if (address >= hot.base && last - hot.base < hot.size) {
            if (write && hot.bytes == nullptr) return false;
            scalar.fragmented = false;
            scalar.contiguous = hot.bytes == nullptr ? nullptr : hot.bytes + address - hot.base;
            return true;
        }
        for (size_t r = 0; r < regions_.size(); ++r) {
            Region& region = regions_[r];
            if (address < region.base || address - region.base >= region.size)
                continue;
            const Cell offset = address - region.base;
            if (width > region.size - offset)
                return false;
            if (region.dense != nullptr) {
                scalar.fragmented = false;
                scalar.contiguous = region.dense + offset;
                hot = HotPage{region.base, region.size, region.dense};
                return true;
            }
            Cell page_index = offset >> page_shift_;
            Cell page_offset = offset & (page_size_ - 1);
            scalar.fragmented = width > page_size_ - page_offset;
            unsigned consumed = 0;
            while (consumed < width) {
                uint8_t* page = nullptr;
                if (!resolve_page(r, page_index, page)) return false;
                if (write && page == nullptr)
                    return false;
                if (!scalar.fragmented) {
                    const Cell page_start = offset - page_offset;
                    const Cell available = region.size - page_start;
                    hot = HotPage{region.base + page_start,
                                  available < page_size_ ? available : page_size_, page};
                    scalar.contiguous = page == nullptr ? nullptr : page + page_offset;
                    return true;
                }
                const unsigned chunk = static_cast<unsigned>(
                    page_size_ - page_offset < width - consumed
                        ? page_size_ - page_offset : width - consumed);
                for (unsigned i = 0; i < chunk; ++i)
                    scalar.bytes[consumed + i] =
                        page == nullptr ? nullptr : page + page_offset + i;
                consumed += chunk;
                ++page_index;
                page_offset = 0;
            }
            return true;
        }
        return false;
    }

    // Stack bounds have already been checked by StackOperation. Reuse a
    // previously qualified, materialized page for the whole operand span.
    // A miss retains scalar resolution, including fragmented/sparse pages.
    uint8_t* stack_span(Cell address, unsigned width) const noexcept {
        const HotPage& hot = address >= return_floor_ && address < return_empty_
            ? return_page_ : data_page_;
        if (hot.bytes != nullptr && address >= hot.base &&
            address - hot.base < hot.size &&
            width <= hot.size - (address - hot.base))
            return hot.bytes + (address - hot.base);
        return nullptr;
    }

    // The hot-page case of resolve(address, width, write, true, ...): a backed
    // span outside the return stack, or null for resolve to decide.
    uint8_t* ordinary_span(Cell address, unsigned width) const noexcept {
        if (address > MASK - (width - 1)) return nullptr;
        const Cell last = address + width - 1;
        if (address < return_empty_ && last >= return_floor_) return nullptr;
        const HotPage& hot = ordinary_page_;
        if (hot.bytes == nullptr || address < hot.base || last - hot.base >= hot.size)
            return nullptr;
        return hot.bytes + (address - hot.base);
    }

    // The part of one stack inside the backed page this run last qualified
    // for it: guest addresses [low, high). It is empty until a stack operation
    // has resolved a cell of that stack, and it never reaches past the stack.
    struct StackWindow {
        Cell low = MASK;
        Cell high = 0;
        std::uintptr_t bias = 0;  // host address of guest address zero
        uint8_t* at(Cell address) const noexcept {
            return reinterpret_cast<uint8_t*>(bias + static_cast<std::uintptr_t>(address));
        }
    };
    StackWindow stack_window(bool returns, Cell floor, Cell empty) const noexcept {
        const HotPage& hot = returns ? return_page_ : data_page_;
        StackWindow window;
        if (hot.bytes == nullptr || hot.size == 0 || hot.size > MASK - hot.base)
            return window;
        const Cell low = hot.base > floor ? hot.base : floor;
        const Cell end = hot.base + hot.size;
        const Cell high = end < empty ? end : empty;
        if (low >= high) return window;
        window.low = low;
        window.high = high;
        window.bias = reinterpret_cast<std::uintptr_t>(hot.bytes) -
                      static_cast<std::uintptr_t>(hot.base);
        return window;
    }

    bool resolve_bytes(Cell address, Cell length, bool require_backing, ByteSpan& span) {
        if (length == 0) return true;
        if (address > MASK - (length - 1)) return false;
        const Cell last = address + length - 1;
        if (address < return_empty_ && last >= return_floor_) return false;
        const auto& hot = ordinary_page_;
        if (address >= hot.base && address - hot.base < hot.size &&
            length <= hot.size - (address - hot.base)) {
            if (require_backing && hot.bytes == nullptr) return false;
            span.append(hot.bytes == nullptr ? nullptr : hot.bytes + address - hot.base,
                        length);
            return true;
        }
        for (size_t r = 0; r < regions_.size(); ++r) {
            const Region& region = regions_[r];
            if (address < region.base || address - region.base >= region.size) continue;
            Cell offset = address - region.base;
            if (length > region.size - offset) return false;
            if (region.dense != nullptr) {
                span.append(region.dense + offset, length);
                ordinary_page_ = HotPage{region.base, region.size, region.dense};
                return true;
            }
            while (length != 0) {
                uint8_t* page = nullptr;
                if (!resolve_page(r, offset >> page_shift_, page) ||
                    (require_backing && page == nullptr)) return false;
                const Cell page_offset = offset & (page_size_ - 1);
                const Cell size = std::min(page_size_ - page_offset, length);
                span.append(page == nullptr ? nullptr : page + page_offset, size);
                const Cell page_start = offset - page_offset;
                ordinary_page_ = HotPage{region.base + page_start,
                    std::min(page_size_, region.size - page_start), page};
                offset += size;
                length -= size;
            }
            return true;
        }
        return false;
    }

private:
    struct HotPage {
        Cell base = 0;
        Cell size = 0;
        uint8_t* bytes = nullptr;
    };
    HotPage data_page_, return_page_, ordinary_page_;
    unsigned page_shift_ = 0;

    bool resolve_page(size_t region_index, Cell page_index, uint8_t*& page) {
        auto& cache = caches_[region_index];
        const auto found = cache.find(page_index);
        if (found != cache.end()) {
            page = found->second;
            return true;
        }
        py::int_ key(page_index);
        PyObject* value = PyDict_GetItemWithError(
            regions_[region_index].pages.ptr(), key.ptr());
        if (value == nullptr && PyErr_Occurred())
            throw py::error_already_set();
        if (value != nullptr) {
            if (!PyByteArray_Check(value) ||
                static_cast<Cell>(PyByteArray_Size(value)) != page_size_)
                return false;
            page = reinterpret_cast<uint8_t*>(PyByteArray_AsString(value));
        }
        cache.emplace(page_index, page);
        return true;
    }

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
    // The caller's plan when this run made the call, so that its return need
    // not look the caller up again. It lives for the run and is not exported.
    Plan* plan = nullptr;
};

// During one native interval RP moves only through checked pushes/pops.
// Index changed slots relative to its entry position, growing separately in
// each direction. This avoids hashing every call/return and never allocates
// storage for the unused portion of a caller-provided return stack.
class ContinuationChanges {
public:
    explicit ContinuationChanges(Cell anchor) : anchor_(anchor) {}

    const ContinuationUpdate* find(Cell slot) const noexcept {
        const auto& side = slot < anchor_ ? below_ : above_;
        const Cell index = index_of(slot);
        if (index >= side.size() || !side[index].touched) return nullptr;
        return &side[index].value;
    }

    void set(Cell slot, ContinuationUpdate value) {
        prepare(slot);
        assign(slot, value);
    }

    void erase_pair(Cell first, Cell second) {
        // Complete both possible allocations before changing either type.
        // Failed growth can leave only untouched scratch entries behind.
        prepare(first);
        prepare(second);
        assign(first, ContinuationUpdate{});
        assign(second, ContinuationUpdate{});
    }

    void append_to(py::list& updates) const {
        auto append = [&](Cell slot, const Change& change) {
            if (change.touched)
                updates.append(py::make_tuple(slot, change.value.caller,
                                              change.value.ip, change.value.raw));
        };
        for (size_t i = 0; i < below_.size(); ++i)
            append(anchor_ - (i + 1) * 8, below_[i]);
        for (size_t i = 0; i < above_.size(); ++i)
            append(anchor_ + i * 8, above_[i]);
    }

private:
    struct Change {
        bool touched = false;
        ContinuationUpdate value{};
    };
    Cell index_of(Cell slot) const noexcept {
        return slot < anchor_ ? (anchor_ - slot) / 8 - 1 : (slot - anchor_) / 8;
    }
    void prepare(Cell slot) {
        auto& side = slot < anchor_ ? below_ : above_;
        const Cell index = index_of(slot);
        if (index >= side.size()) grow(side, index + 1);
    }
    // Growth is rare and may throw; it stays out of the callers' hot paths.
    [[gnu::noinline, gnu::cold]] static void grow(std::vector<Change>& side, Cell size) {
        side.resize(size);
    }
    void assign(Cell slot, ContinuationUpdate value) noexcept {
        auto& side = slot < anchor_ ? below_ : above_;
        side[index_of(slot)] = Change{true, value};
    }
    Cell anchor_;
    std::vector<Change> below_, above_;
};

// How a native interval leaves machine work for Python, if it does.
enum MachineHandoff : uint32_t {
    MACHINE_NONE,
    // A routine call ran but stopped short of returning or calling back.
    MACHINE_STOPPED_CALL,
    // The newest entry, resumed after a callback, yielded or failed.
    MACHINE_STOPPED_RESUME,
    // The newest entry stopped at a callback that Python runs.
    MACHINE_CALLBACK,
    // A callback finished inline; Python resumes the newest entry.
    MACHINE_RETURN,
};

// A routine entry begun by this interval and not yet returned. While its
// callback runs here, ``slot`` is the return-stack cell holding the machine's
// CALL.L return address; the callback word returns into it.
struct MachineFrame {
    Cell routine;              // the called routine's index
    Cell caller_xt, caller_ip; // where Forth continues once it returns
    Cell frontier;             // the return-stack pointer it was called with
    bool in_callback = false;
    Cell site_routine = 0;     // the routine whose site is calling back
    Cell site = 0;
    Cell slot = 0;
    Cell raw = 0;              // the return address held in the slot
    Cell depth = 0;            // data depth below the callback's arguments
};

struct RunState {
    Cell xt;
    Cell ip;
    Cell steps = 0;
    StackState data;
    StackState returns;
    Cell cookie;
    Cell fpcsr;
    ContinuationChanges changed{returns.pointer};
    // Tile registers: TMODE, TSRC0, TSRC1 and TDST, when this run admits the
    // tile words, and the tile operations completed for the accountant.
    bool tile = false;
    // The owner's guard runs once, at the first tile word of a run.
    bool tile_checked = false;
    bool tile_allowed = false;
    Cell tile_mode = 0;
    Cell tile_source0 = 0;
    Cell tile_source1 = 0;
    Cell tile_destination = 0;
    Cell tile_operations = 0;
    Cell pointer_captures = 0;
    // The step allowance, and the cost of the operation being executed, for
    // callbacks that run inline within one operation.
    Cell step_limit = 0;
    Cell operation_cost = 0;
    // Machine routines called during this interval.
    Cell machine_allowance = 0;
    Cell machine_instructions = 0;
    Cell machine_calls = 0;
    Cell machine_callbacks = 0;
    uint32_t machine_handoff = MACHINE_NONE;
    std::vector<MachineFrame> frames{};
    megapad::hybrid::RoutineCallbackStop callback{};
};

// A declared machine routine the executor may call through the hybrid
// runner. Its buffer rules turn argument cells into borrowed spans.
struct RoutineRule {
    unsigned address_argument;
    unsigned length_argument;
    Cell element_bytes;
    Cell max_bytes;  // zero when the rule has no maximum
    uint32_t access;
};

// A callback site and the word bound to it: OP_CALL to a planned colon word,
// or one operation run inline. OP_STOP until Python binds the site's word.
struct RoutineSite {
    unsigned inputs = 0;
    unsigned outputs = 0;
    uint32_t opcode = OP_STOP;
    Cell a = 0;
    Cell b = 0;
};

struct NativeRoutine {
    std::uintptr_t image = 0;
    unsigned inputs = 0;
    unsigned outputs = 0;
    std::vector<RoutineRule> rules;
    std::vector<RoutineSite> sites;
};

// Operations a callback may run inline: they touch neither the return stack
// nor the instruction stream.
static bool inline_callback(uint32_t opcode) noexcept {
    switch (opcode) {
    case OP_STOP: case OP_LITERAL: case OP_BRANCH: case OP_BRANCH_ZERO:
    case OP_CALL: case OP_RETURN: case OP_STORE_VALUE: case OP_STRING_LITERAL:
    case OP_R_PUSH: case OP_R_POP: case OP_R_PEEK:
    case OP_DO: case OP_QUESTION_DO: case OP_LOOP: case OP_PLUS_LOOP: case OP_UNLOOP:
    case OP_R_PUSH_PAIR: case OP_R_POP_PAIR: case OP_R_PEEK_PAIR: case OP_ABORT_IF:
    case OP_I: case OP_J: case OP_EXECUTE: case OP_RP_FETCH: case OP_CALL_ROUTINE:
        return false;
    default:
        return opcode <= OP_CALL_ROUTINE;
    }
}

static Cell flag(bool value) noexcept { return value ? MASK : 0; }
static bool less_signed(Cell left, Cell right) noexcept {
    return (left ^ SIGN) < (right ^ SIGN);
}
static Cell magnitude(Cell value) noexcept {
    return value & SIGN ? Cell{0} - value : value;
}

static constexpr unsigned cost(uint32_t opcode) noexcept {
    return opcode >= OP_LITERAL && opcode <= OP_ABORT_IF ? 1 : 2;
}

// The value of an operation that replaces the top cell with a function of
// it. False for any other opcode.
[[gnu::always_inline]] static inline bool unary_value(uint32_t opcode, Cell operand,
                                                       Cell& value) noexcept {
    switch (opcode) {
    case OP_NEGATE: value = Cell{0} - operand; return true;
    case OP_ABS: value = magnitude(operand); return true;
    case OP_ONE_PLUS: value = operand + 1; return true;
    case OP_ONE_MINUS: value = operand - 1; return true;
    case OP_TWO_MULTIPLY: value = operand << 1; return true;
    case OP_CELLS: value = operand << 3; return true;
    case OP_CELL_PLUS: value = operand + Cell{8}; return true;
    case OP_TWO_DIVIDE: value = (operand >> 1) | (operand & SIGN); return true;
    case OP_INVERT: value = ~operand; return true;
    case OP_ZERO_EQUAL: value = flag(operand == 0); return true;
    case OP_ZERO_NOT_EQUAL: value = flag(operand != 0); return true;
    case OP_ZERO_LESS: value = flag((operand & SIGN) != 0); return true;
    case OP_ZERO_GREATER: value = flag(operand != 0 && !(operand & SIGN)); return true;
    case OP_BSWAP:
        value = 0;
        for (unsigned i = 0; i < 8; ++i)
            value |= ((operand >> (i * 8)) & 255) << ((7 - i) * 8);
        return true;
    default: return false;
    }
}

// The value of an operation that replaces the top two cells with one and
// cannot fault. ``left`` is the deeper operand. False for any other opcode.
[[gnu::always_inline]] static inline bool binary_value(uint32_t opcode, Cell left, Cell right,
                                                        Cell& value) noexcept {
    switch (opcode) {
    case OP_ADD: value = left + right; return true;
    case OP_SUBTRACT: value = left - right; return true;
    case OP_MULTIPLY: value = left * right; return true;
    case OP_MIN: value = less_signed(right, left) ? right : left; return true;
    case OP_MAX: value = less_signed(left, right) ? right : left; return true;
    case OP_AND: value = left & right; return true;
    case OP_OR: value = left | right; return true;
    case OP_XOR: value = left ^ right; return true;
    case OP_LSHIFT: value = left << (right & 63); return true;
    case OP_RSHIFT: value = left >> (right & 63); return true;
    case OP_EQUAL: value = flag(left == right); return true;
    case OP_NOT_EQUAL: value = flag(left != right); return true;
    case OP_ULESS: value = flag(left < right); return true;
    case OP_UGREATER: value = flag(left > right); return true;
    case OP_LESS: value = flag(less_signed(left, right)); return true;
    case OP_LESS_EQUAL: value = flag(!less_signed(right, left)); return true;
    case OP_GREATER: value = flag(less_signed(right, left)); return true;
    case OP_GREATER_EQUAL: value = flag(!less_signed(left, right)); return true;
    default: return false;
    }
}

// The cell a call leaves on the return stack: the next cookie that cannot be
// taken for the caller's own execution token. False, with nothing changed,
// once the sequence is spent.
static bool next_continuation_cookie(Cell& cookie, Cell caller, Cell& raw) noexcept {
    Cell next = cookie;
    do {
        if (next == MASK) return false;
        ++next;
        raw = 0xC07ECAFE00000000ULL ^ next;
    } while (raw == caller);
    cookie = next;
    return true;
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
#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
        if (count == 0) return true;
        if (auto* bytes = memory_.stack_span(state_.pointer, count * 8)) {
            for (unsigned i = 0; i < count; ++i)
                std::memcpy(&values[i], bytes + i * 8, sizeof(Cell));
            return true;
        }
#endif
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
#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
        if (count == 0) return true;
        contiguous_output_ = memory_.stack_span(destination_, count * 8);
        if (contiguous_output_ != nullptr) return true;
#endif
        for (unsigned i = 0; i < count; ++i) {
            const Cell address = destination_ + (count - 1 - i) * 8;
            if (!memory_.resolve(address, 8, true, false, output_[i]))
                return false;
        }
        return true;
    }

    void commit() noexcept {
        if (contiguous_output_ != nullptr) {
            for (unsigned i = 0; i < produced_; ++i)
                std::memcpy(contiguous_output_ + (produced_ - 1 - i) * 8,
                            &result[i], sizeof(Cell));
        } else {
            for (unsigned i = 0; i < produced_; ++i)
                output_[i].write(result[i], 8);
        }
        state_.pointer = destination_;
    }

    // The pointer passes ``cells`` cells, which keep their bytes.
    void release(unsigned cells) noexcept { state_.pointer += Cell{8} * cells; }
    Cell depth() const noexcept { return state_.depth(); }
    // The cell ``index`` cells below the top; the caller has checked the depth.
    bool deep(Cell index, Cell& value) {
        Scalar scalar;
        if (!memory_.resolve(state_.pointer + index * 8, 8, false, false, scalar))
            return false;
        value = scalar.read(8);
        return true;
    }

    // inputs() and the operation initialize every consumed/produced cell.
    // Avoid clearing scratch and six scatter descriptors at every operation.
    std::array<Cell, 4> values;
    std::array<Cell, 6> result;

private:
    MemoryRun& memory_;
    StackState& state_;
    unsigned consumed_ = 0;
    unsigned produced_ = 0;
    Cell destination_ = 0;
    uint8_t* contiguous_output_ = nullptr;
    std::array<Scalar, 6> output_;
};

#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define MEGAFORTH_DIRECT_PATH 1

// StackOperation's work for cells a run has already qualified: the top of one
// stack, read and written in place inside its window. It declines whatever
// reaches outside the window. The window lies within the stack, so its checks
// also prove the depth and the room that StackOperation checks.
//
// It holds copies of the window and the pointer and hands the moved pointer
// back, so that nothing here refers to the caller's own variables and they
// can stay in host registers.
class WindowOperation {
public:
    WindowOperation(const MemoryRun::StackWindow& window, Cell pointer) noexcept
        : window_(window), pointer_(pointer) {}

    // Cells are copied by number, never in a loop: each index below is a
    // constant, which is what lets the compiler keep the cells in registers.
    bool inputs(unsigned count) noexcept {
        consumed_ = count;
        if (pointer_ < window_.low || pointer_ > window_.high ||
            window_.high - pointer_ < Cell{8} * count)
            return false;
        switch (count) {
        case 4: std::memcpy(&values[3], window_.at(pointer_ + 24), sizeof(Cell)); [[fallthrough]];
        case 3: std::memcpy(&values[2], window_.at(pointer_ + 16), sizeof(Cell)); [[fallthrough]];
        case 2: std::memcpy(&values[1], window_.at(pointer_ + 8), sizeof(Cell)); [[fallthrough]];
        case 1: std::memcpy(&values[0], window_.at(pointer_), sizeof(Cell)); [[fallthrough]];
        default: return true;
        }
    }

    bool outputs(unsigned count) noexcept {
        produced_ = count;
        after_pop_ = pointer_ + Cell{8} * consumed_;
        return after_pop_ - window_.low >= Cell{8} * count;
    }

    // Store the outputs, the first of them deepest. Returns the pointer,
    // which now stands at the last.
    Cell apply() noexcept {
        switch (produced_) {
        case 6: std::memcpy(window_.at(after_pop_ - 48), &result[5], sizeof(Cell)); [[fallthrough]];
        case 5: std::memcpy(window_.at(after_pop_ - 40), &result[4], sizeof(Cell)); [[fallthrough]];
        case 4: std::memcpy(window_.at(after_pop_ - 32), &result[3], sizeof(Cell)); [[fallthrough]];
        case 3: std::memcpy(window_.at(after_pop_ - 24), &result[2], sizeof(Cell)); [[fallthrough]];
        case 2: std::memcpy(window_.at(after_pop_ - 16), &result[1], sizeof(Cell)); [[fallthrough]];
        case 1: std::memcpy(window_.at(after_pop_ - 8), &result[0], sizeof(Cell)); [[fallthrough]];
        default: break;
        }
        pointer_ = after_pop_ - Cell{8} * produced_;
        return pointer_;
    }

    // Returns the pointer moved past ``cells`` cells, which keep their bytes.
    Cell pass(unsigned cells) noexcept {
        pointer_ += Cell{8} * cells;
        return pointer_;
    }

    // Cells from the pointer to the window's end, once inputs() has passed.
    Cell depth() const noexcept { return (window_.high - pointer_) / 8; }
    bool deep(Cell index, Cell& value) const noexcept {
        std::memcpy(&value, window_.at(pointer_ + index * 8), sizeof(Cell));
        return true;
    }

    std::array<Cell, 4> values;
    std::array<Cell, 6> result;

private:
    const MemoryRun::StackWindow window_;
    Cell pointer_;
    unsigned consumed_ = 0;
    unsigned produced_ = 0;
    Cell after_pop_ = 0;
};
// The two stacks' accessors are distinct types, so that a path can tell
// whose pointer an accessor has moved.
struct DataWindow : WindowOperation { using WindowOperation::WindowOperation; };
struct ReturnWindow : WindowOperation { using WindowOperation::WindowOperation; };

// One ordinary scalar inside the hot page, read and written as Scalar is.
struct DirectCell {
    uint8_t* bytes = nullptr;

    Cell read(unsigned width) const noexcept {
        switch (width) {
        case 1: return *bytes;
        case 2: { uint16_t value; std::memcpy(&value, bytes, sizeof(value)); return value; }
        case 4: { uint32_t value; std::memcpy(&value, bytes, sizeof(value)); return value; }
        default: { Cell value; std::memcpy(&value, bytes, sizeof(value)); return value; }
        }
    }

    void write(Cell value, unsigned width) const noexcept {
        switch (width) {
        case 1: *bytes = static_cast<uint8_t>(value); return;
        case 2: { const auto low = static_cast<uint16_t>(value); std::memcpy(bytes, &low, sizeof(low)); return; }
        case 4: { const auto low = static_cast<uint32_t>(value); std::memcpy(bytes, &low, sizeof(low)); return; }
        default: std::memcpy(bytes, &value, sizeof(value)); return;
        }
    }
};
#endif

class NativeProgram {
public:
    NativeProgram(const py::iterable& regions, Cell page_size,
                  py::object continuation_type, const py::iterable& dense_regions)
        : page_size_(page_size), continuation_type_(std::move(continuation_type)),
          regions_(megapad::guest::build_regions(regions, page_size, dense_regions)) {}

    // An overlaid plan belongs to a word with a Python execution overlay. It
    // runs only where Python enters it, after the overlay declined; a native
    // EXECUTE never enters it, so the overlay keeps every entry decision.
    void install(Cell xt, const py::iterable& operations, bool overlaid) {
        if (xt == 0)
            throw py::value_error("plan execution token must be nonzero");
        std::vector<Instruction> plan;
        for (py::handle item : operations) {
            auto operation = py::cast<py::tuple>(item);
            if (operation.size() != 3)
                throw py::value_error("operation must be (opcode, a, b)");
            const auto opcode = operation[0].cast<uint32_t>();
            if (opcode > OP_DOES)
                throw py::value_error("unknown native semantic opcode");
            plan.push_back(Instruction{opcode, OP_STOP, operation[1].cast<Cell>(),
                                      operation[2].cast<Cell>()});
            if (opcode == OP_CALL || opcode == OP_EXECUTE)
                plan.back().target = nullptr;
            if (opcode == OP_DOES) {
                const Cell index = plan.back().b;
                if (index >= does_.size())
                    throw py::value_error("OP_DOES names no installed DOES> entry");
                plan.back().does = &does_[index];
            }
        }
        for (size_t i = 0; i + 1 < plan.size(); ++i) {
            auto& first = plan[i];
            const auto next = plan[i + 1].opcode;
            if (first.opcode == OP_DUP && next == OP_BRANCH_ZERO) {
                first.fused = next;
            } else if (first.opcode == OP_LITERAL || first.opcode == OP_PUSH_CELL) {
                switch (next) {
                case OP_ADD: case OP_SUBTRACT: case OP_MULTIPLY:
                case OP_AND: case OP_OR: case OP_XOR:
                case OP_LSHIFT: case OP_RSHIFT:
                case OP_EQUAL: case OP_NOT_EQUAL: case OP_ULESS: case OP_UGREATER:
                case OP_LESS: case OP_LESS_EQUAL: case OP_GREATER: case OP_GREATER_EQUAL:
                case OP_FETCH: case OP_C_FETCH: case OP_W_FETCH: case OP_L_FETCH:
                    first.fused = next;
                    break;
                }
            }
        }
        plans_.insert_or_assign(xt, std::move(plan));
        if (overlaid) overlaid_.insert(xt);
        else overlaid_.erase(xt);
    }

    void clear() {
        plans_.clear();
        overlaid_.clear();
        does_.clear();
    }

    // Cycle collection (collected.h). The program owns the continuation
    // type, the tile guard, the routine capsule and its regions' pages and
    // leases. Through the guard it holds the machine's tile service and
    // memory.
    int gc_traverse(visitproc visit, void* arg) const noexcept {
        Py_VISIT(continuation_type_.ptr());
        Py_VISIT(tile_guard_.ptr());
        Py_VISIT(routine_entry_.ptr());
        return megapad::guest::traverse_regions(regions_, visit, arg);
    }

    // Afterwards the program holds no plan, routine or region: a run finds
    // nothing planned and returns to Python at once.
    void gc_clear() noexcept {
        clear();
        bind_routines(py::none());
        py::object guard = std::move(tile_guard_);
        py::object continuation_type = std::move(continuation_type_);
        megapad::guest::release_regions(regions_);
    }

    // Record where the children of one DOES> continue and return the index a
    // plan names it by. The defining word's plan is found at the first call.
    Cell does_entry(Cell xt, Cell ip) {
        if (xt == 0)
            throw py::value_error("DOES> source execution token must be nonzero");
        does_.push_back(DoesEntry{xt, ip});
        return does_.size() - 1;
    }

    // Steps run on the direct path since this program was made: how much
    // of the native work took it. Zero where the direct path is not built.
    Cell direct_steps() const noexcept { return direct_steps_; }

    // The owner's check that the tile service still computes with this
    // extension's kernel, called at most once per run, at its first tile word.
    void bind_tile_guard(py::object guard) { tile_guard_ = std::move(guard); }

    // Bind the hybrid runner's call capsule, or None to unbind. Routine
    // indexes are installed afterwards with set_routine().
    void bind_routines(py::object entry) {
        routines_.clear();
        routine_call_ = nullptr;
        routine_entry_ = py::object();
        if (entry.is_none())
            return;
        if (!PyCapsule_IsValid(entry.ptr(), megapad::hybrid::ROUTINE_CALL_CAPSULE))
            throw py::type_error("routine entry must be the hybrid runner's call capsule");
        routine_call_ = static_cast<const megapad::hybrid::RoutineCall*>(
            PyCapsule_GetPointer(entry.ptr(), megapad::hybrid::ROUTINE_CALL_CAPSULE));
        routine_entry_ = std::move(entry);
    }

    void set_routine(Cell index, std::uintptr_t image, unsigned inputs, unsigned outputs,
                     const py::iterable& rules, const py::iterable& sites) {
        if (inputs > 8 || outputs > 8)
            throw py::value_error("routines pass at most eight cells");
        NativeRoutine routine{image, inputs, outputs, {}, {}};
        for (py::handle item : sites) {
            auto site = py::cast<py::tuple>(item);
            if (site.size() != 2)
                throw py::value_error("site must be (input_cells, output_cells)");
            RoutineSite declared;
            declared.inputs = site[0].cast<unsigned>();
            declared.outputs = site[1].cast<unsigned>();
            if (declared.inputs > 8 || declared.outputs > 8)
                throw py::value_error("callbacks pass at most eight cells");
            routine.sites.push_back(declared);
        }
        for (py::handle item : rules) {
            auto rule = py::cast<py::tuple>(item);
            if (rule.size() != 5)
                throw py::value_error("rule must be (address, length, element_bytes, max_bytes, access)");
            const auto address = rule[0].cast<unsigned>();
            const auto length = rule[1].cast<unsigned>();
            if (address >= inputs || length >= inputs)
                throw py::value_error("rule names an argument the routine does not take");
            if (rule[2].cast<Cell>() == 0)
                throw py::value_error("rule element size must be positive");
            routine.rules.push_back(RoutineRule{address, length, rule[2].cast<Cell>(),
                                                rule[3].cast<Cell>(), rule[4].cast<uint32_t>()});
        }
        if (index > routines_.size())
            throw py::value_error("routine indexes are installed in order");
        if (index == routines_.size()) routines_.push_back(std::move(routine));
        else routines_[index] = std::move(routine);
    }

    // Bind a callback site of an installed routine to a planned colon word
    // (OP_CALL) or to one inline operation; OP_STOP leaves it to Python.
    void set_routine_target(Cell index, Cell site, uint32_t opcode, Cell a, Cell b) {
        if (index >= routines_.size() || site >= routines_[index].sites.size())
            throw py::value_error("no such routine callback site");
        if (opcode != OP_STOP && opcode != OP_CALL && !inline_callback(opcode))
            throw py::value_error("a callback runs a colon word or one inline operation");
        RoutineSite& target = routines_[index].sites[site];
        target.opcode = opcode;
        target.a = a;
        target.b = b;
    }

    py::object snapshot_stack(const std::array<Cell, 3>& bounds,
                              const py::object& continuations) {
        const StackState stack{bounds[0], bounds[1], bounds[2]};
        if (!stack.valid()) return py::none();
        const bool typed = !continuations.is_none();
        if (typed && !PyDict_CheckExact(continuations.ptr())) return py::none();
        MemoryRun memory(regions_, page_size_, stack.floor, stack.empty);
        py::tuple snapshot(stack.depth());
        std::vector<Cell> stale;
        for (Cell index = 0, address = stack.pointer; address < stack.empty;
             ++index, address += sizeof(Cell)) {
            Scalar scalar;
            if (!memory.resolve(address, sizeof(Cell), false, false, scalar))
                return py::none();
            const Cell raw = scalar.read(sizeof(Cell));
            PyObject* entry = nullptr;
            if (typed) {
                py::int_ key(address);
                PyObject* found = PyDict_GetItemWithError(continuations.ptr(), key.ptr());
                if (found == nullptr && PyErr_Occurred()) throw py::error_already_set();
                if (found != nullptr) {
                    // Custom metadata retains the reference snapshot path.
                    // Exact continuation objects are immutable and can be
                    // retained directly, including roots and fault frames.
                    if (!PyTuple_CheckExact(found) || PyTuple_GET_SIZE(found) != 2)
                        return py::none();
                    PyObject* frame = PyTuple_GET_ITEM(found, 0);
                    PyObject* cookie = PyTuple_GET_ITEM(found, 1);
                    if (reinterpret_cast<PyObject*>(Py_TYPE(frame)) != continuation_type_.ptr() ||
                        !PyLong_CheckExact(cookie)) return py::none();
                    const Cell expected = PyLong_AsUnsignedLongLong(cookie);
                    if (PyErr_Occurred()) {
                        PyErr_Clear();
                        return py::none();
                    }
                    if (raw == expected) entry = Py_NewRef(frame);
                    else stale.push_back(address);
                }
            }
            if (entry == nullptr) entry = PyLong_FromUnsignedLongLong(raw);
            if (entry == nullptr) throw py::error_already_set();
            PyTuple_SET_ITEM(snapshot.ptr(), stack.depth() - index - 1, entry);
        }
        // Preflight the entire snapshot before removing stale active types.
        // Inactive retained slots remain untouched for a later RP!.
        for (Cell address : stale) {
            py::int_ key(address);
            if (PyDict_DelItem(continuations.ptr(), key.ptr()) != 0)
                throw py::error_already_set();
        }
        return snapshot;
    }

    py::tuple run(Cell xt, Cell ip, const std::array<Cell, 3>& data_state,
                  const py::tuple& return_state, const py::dict& continuations,
                  Cell remaining_steps, Cell fpcsr, Cell machine_allowance,
                  const py::object& tile_state) {
        if (return_state.size() != 4)
            throw py::value_error("return state must contain four cells");
        RunState state{xt, ip, 0,
            StackState{data_state[0], data_state[1], data_state[2]},
            StackState{return_state[0].cast<Cell>(), return_state[1].cast<Cell>(),
                       return_state[2].cast<Cell>()}, 0, fpcsr};
        // Python's continuation sequence is deliberately unbounded. Decline
        // very large sequences rather than wrap or truncate their identity.
        try {
            state.cookie = return_state[3].cast<Cell>();
        } catch (const py::cast_error&) {
            return py::make_tuple(xt, ip, 0, state.data.pointer,
                state.returns.pointer, return_state[3], py::list(), 0, fpcsr, py::none(),
                tile_state);
        }
        state.machine_allowance = machine_allowance;
        state.step_limit = remaining_steps;
        if (!tile_state.is_none()) {
            const auto tile = tile_state.cast<std::array<Cell, 4>>();
            state.tile = true;
            state.tile_mode = tile[0];
            state.tile_source0 = tile[1];
            state.tile_source1 = tile[2];
            state.tile_destination = tile[3];
        }
        if (!state.data.valid() || !state.returns.valid() ||
            !(state.data.empty <= state.returns.floor ||
              state.returns.empty <= state.data.floor))
            return result(state);
        MemoryRun memory(regions_, page_size_, state.returns.floor,
                         state.returns.empty);
        try {
            const auto initial = plans_.find(state.xt);
            Plan* plan = initial == plans_.end() ? nullptr : &initial->second;
            GeneralPath general(*this, memory, state, continuations, plan);
            while (state.steps < remaining_steps) {
                if (plan == nullptr) break;
#ifdef MEGAFORTH_DIRECT_PATH
                // A callback's word returns into the machine through a slot
                // only return_slot knows, so the direct path stays outside
                // callbacks.
                if (state.frames.empty()) {
                    const Cell before = state.steps;
                    plan = run_direct(state, memory, plan, remaining_steps);
                    direct_steps_ += state.steps - before;
                    if (state.steps >= remaining_steps) break;
                }
#endif
                if (state.ip >= plan->size()) break;
                if (!step(general, (*plan)[state.ip], remaining_steps)) break;
                // Machine work this interval cannot finish goes to Python.
                if (state.machine_handoff != MACHINE_NONE)
                    break;
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
        state.changed.append_to(updates);
        py::object machine = py::none();
        if (state.machine_calls != 0 || state.machine_handoff != MACHINE_NONE ||
                !state.frames.empty()) {
            // Open entries, oldest first, for Python to adopt as its own.
            py::list frames;
            for (const MachineFrame& frame : state.frames)
                frames.append(py::make_tuple(frame.routine, frame.caller_xt, frame.caller_ip,
                    frame.frontier, frame.in_callback, frame.site_routine, frame.site,
                    frame.slot, frame.raw, frame.depth));
            py::object callback = py::none();
            if (state.machine_handoff == MACHINE_CALLBACK) {
                const auto& stop = state.callback;
                py::tuple arguments(stop.argument_count);
                for (Cell i = 0; i < stop.argument_count; ++i)
                    arguments[i] = py::int_(stop.arguments[i]);
                callback = py::make_tuple(static_cast<Cell>(stop.image), stop.site, stop.slot,
                                          arguments);
            }
            machine = py::make_tuple(state.machine_instructions, state.machine_calls,
                state.machine_callbacks, state.machine_handoff, frames, callback);
        }
        py::object tile = py::none();
        if (state.tile)
            tile = py::make_tuple(state.tile_mode, state.tile_source0, state.tile_source1,
                                  state.tile_destination, state.tile_operations);
        return py::make_tuple(state.xt, state.ip, state.steps,
            state.data.pointer, state.returns.pointer, state.cookie, updates,
            state.pointer_captures, state.fpcsr, machine, tile);
    }

    // TADD, TSUB and TMUL as HostedTileService._binary performs them: the
    // mode must name a defined format, both sources are read before the
    // destination is written, and the value kernel is the one the Python
    // service calls. Any span outside ordinary backed memory declines.
    [[gnu::noinline]] static bool execute_tile_binary(const Instruction& operation,
                                                      MemoryRun& memory, RunState& s,
                                                      StackOperation& stack) {
        namespace tile = megapad::tile_values;
        if (!s.tile || (s.tile_mode & 0xF) > 7 || operation.a > TILE_MULTIPLY) return false;
        if (!stack.inputs(0) || !stack.outputs(0)) return false;
        ByteSpan left, right, destination;
        if (!memory.resolve_bytes(s.tile_source0, 64, false, left) ||
            !memory.resolve_bytes(s.tile_source1, 64, false, right) ||
            !memory.resolve_bytes(s.tile_destination, 64, true, destination)) return false;
        std::array<uint8_t, 64> first{}, second{};
        left.read_into(first.data());
        right.read_into(second.data());
        const auto selected = operation.a == TILE_ADD ? tile::Operation::Add :
            operation.a == TILE_SUBTRACT ? tile::Operation::Subtract :
            tile::Operation::Multiply;
        const auto mode = static_cast<unsigned>(s.tile_mode);
        if (!tile::supported(selected, mode)) return false;
        const auto outcome = tile::execute(selected, mode,
            tile::ByteView{first.data(), first.size()},
            tile::ByteView{second.data(), second.size()});
        if (outcome.kind != tile::ResultKind::Bytes || outcome.size != 64) return false;
        stack.commit();
        destination.write_from(outcome.bytes.data());
        ++s.tile_operations;
        ++s.ip;
        return true;
    }

    // Call a machine routine directly. Anything the fast path cannot admit
    // is left untouched for Python, which makes the same call itself.
    bool call_routine(const Instruction& operation, MemoryRun& memory, RunState& s,
                      const py::dict& continuations, Plan*& plan) {
        if (routine_call_ == nullptr || operation.a >= routines_.size() ||
                s.machine_allowance == 0)
            return false;
        const NativeRoutine& routine = routines_[operation.a];
        const unsigned inputs = routine.inputs;
        const unsigned outputs = routine.outputs;
        if (s.data.depth() < inputs) return false;
        const Cell after_pop = s.data.pointer + Cell(inputs) * 8;
        if (after_pop - s.data.floor < Cell(outputs) * 8) return false;
        std::array<uint64_t, 8> arguments{};
        for (unsigned i = 0; i < inputs; ++i) {
            // The deepest input cell is the routine's first argument.
            Scalar cell;
            if (!memory.resolve(s.data.pointer + Cell(inputs - 1 - i) * 8, 8, false, false, cell))
                return false;
            arguments[i] = cell.read(8);
        }
        std::array<Scalar, 8> destinations;
        for (unsigned i = 0; i < outputs; ++i) {
            if (!memory.resolve(after_pop - Cell(i + 1) * 8, 8, true, false, destinations[i]))
                return false;
        }
        std::array<megapad::hybrid::RoutineSpan, 16> spans;
        std::size_t span_count = 0;
        for (const RoutineRule& rule : routine.rules) {
            const Cell base = arguments[rule.address_argument];
            const Cell count = arguments[rule.length_argument];
            if (count > MASK / rule.element_bytes) return false;
            const Cell size = count * rule.element_bytes;
            if (rule.max_bytes != 0 && size > rule.max_bytes) return false;
            if (size == 0) continue;
            if (span_count == spans.size()) return false;
            spans[span_count++] = megapad::hybrid::RoutineSpan{base, size, rule.access};
        }
        std::array<uint64_t, 8> results{};
        megapad::hybrid::RoutineCallbackStop callback{};
        uint64_t instructions = 0;
        const int outcome = routine_call_->call(routine_call_->runner, routine.image,
            arguments.data(), inputs, spans.data(), span_count, s.returns.pointer,
            s.returns.floor, s.machine_allowance, results.data(), outputs, &callback,
            &instructions);
        if (outcome == megapad::hybrid::ROUTINE_DECLINED) return false;
        account_machine(s, instructions);
        ++s.machine_calls;
        s.data.pointer = after_pop;
        if (outcome == megapad::hybrid::ROUTINE_RETURNED) {
            for (unsigned i = 0; i < outputs; ++i)
                destinations[i].write(results[i], 8);
            s.data.pointer = after_pop - Cell(outputs) * 8;
            ++s.ip;
            return true;
        }
        if (outcome != megapad::hybrid::ROUTINE_CALLBACK) {
            // The call consumed its inputs and stays at this IP for Python.
            s.machine_handoff = MACHINE_STOPPED_CALL;
            return true;
        }
        MachineFrame frame;
        frame.routine = operation.a;
        frame.caller_xt = s.xt;
        frame.caller_ip = s.ip + 1;
        frame.frontier = s.returns.pointer;
        s.frames.push_back(frame);
        return enter_callback(callback, memory, s, continuations, plan);
    }

    static void account_machine(RunState& s, uint64_t instructions) noexcept {
        s.machine_instructions += instructions;
        s.machine_allowance -= std::min<Cell>(instructions, s.machine_allowance);
    }

    // Start the callback the newest entry stopped at, on the caller's own
    // stacks: the return stack's top becomes the slot holding the machine's
    // return address, the site's argument cells are pushed, and the bound word
    // runs. A colon word returns into the slot; an inline operation returns
    // at once. Anything else leaves the callback for Python to start.
    bool enter_callback(const megapad::hybrid::RoutineCallbackStop& callback, MemoryRun& memory,
                        RunState& s, const py::dict& continuations, Plan*& plan) {
        MachineFrame& frame = s.frames.back();
        auto python_runs_it = [&]() {
            s.callback = callback;
            s.machine_handoff = MACHINE_CALLBACK;
            return true;
        };
        Cell site_routine = routines_.size();
        for (Cell i = 0; i < routines_.size(); ++i)
            if (routines_[i].image == callback.image) site_routine = i;
        if (site_routine == routines_.size() ||
                callback.site >= routines_[site_routine].sites.size())
            return python_runs_it();
        const RoutineSite& site = routines_[site_routine].sites[callback.site];
        const bool inline_operation = site.opcode != OP_CALL;
        if (site.opcode == OP_STOP || callback.argument_count != site.inputs ||
                callback.slot % 8 != 0 || callback.slot < s.returns.floor ||
                callback.slot >= frame.frontier)
            return python_runs_it();
        Plan* target = nullptr;
        if (!inline_operation) {
            const auto found = plans_.find(site.a);
            if (found == plans_.end() || found->second.empty()) return python_runs_it();
            target = &found->second;
        } else if (s.steps + s.operation_cost + 1 > s.step_limit) {
            // The inline word's own step would pass this interval's allowance.
            return python_runs_it();
        }
        Scalar slot;
        if (!memory.resolve(callback.slot, 8, false, false, slot)) return python_runs_it();
        // Preflight the argument cells before any effect.
        if (s.data.pointer - s.data.floor < Cell(site.inputs) * 8) return python_runs_it();
        std::array<Scalar, 8> pushed;
        for (unsigned i = 0; i < site.inputs; ++i) {
            if (!memory.resolve(s.data.pointer - Cell(i + 1) * 8, 8, true, false, pushed[i]))
                return python_runs_it();
        }
        const Cell data_pointer = s.data.pointer;
        const Cell return_pointer = s.returns.pointer;
        frame.in_callback = true;
        frame.site_routine = site_routine;
        frame.site = callback.site;
        frame.slot = callback.slot;
        frame.raw = slot.read(8);
        frame.depth = s.data.depth();
        s.returns.pointer = callback.slot;
        // The first argument cell is the deepest.
        for (unsigned i = 0; i < site.inputs; ++i)
            pushed[i].write(callback.arguments[i], 8);
        s.data.pointer = data_pointer - Cell(site.inputs) * 8;
        ++s.machine_callbacks;
        if (!inline_operation) {
            s.xt = site.a;
            s.ip = 0;
            plan = target;
            return true;
        }
        Instruction operation{site.opcode, OP_STOP, site.a, {site.b}};
        const Cell xt = s.xt, ip = s.ip;
        Plan* unchanged = plan;
        const Cell steps = s.steps;
        GeneralPath inline_path(*this, memory, s, continuations, unchanged);
        if (!effect(inline_path, operation)) {
            // Nothing ran: undo the start and let Python run the callback.
            frame.in_callback = false;
            s.returns.pointer = return_pointer;
            s.data.pointer = data_pointer;
            --s.machine_callbacks;
            return python_runs_it();
        }
        s.xt = xt;
        s.ip = ip;
        // The machine made the call, so the word costs only its own tick.
        s.steps = steps + 1;
        return return_into_machine(memory, s, continuations, plan, true);
    }

    // The callback has returned into the newest entry's slot: hand the site's
    // output cells to the machine and continue it. A routine that returns
    // gives its output cells to its caller, which continues after the call.
    // ``inline_operation`` says no return operation is pending, so a resume
    // that cannot happen here goes to Python as a machine return.
    [[gnu::noinline]] bool return_into_machine(MemoryRun& memory, RunState& s,
                                               const py::dict& continuations,
                             Plan*& plan, bool inline_operation) {
        MachineFrame& frame = s.frames.back();
        const NativeRoutine& routine = routines_[frame.routine];
        const RoutineSite& site = routines_[frame.site_routine].sites[frame.site];
        auto python_resumes = [&]() {
            if (!inline_operation) return false;
            s.machine_handoff = MACHINE_RETURN;
            return true;
        };
        if (s.machine_allowance == 0 || s.data.depth() != frame.depth + site.outputs)
            return python_resumes();
        std::array<uint64_t, 8> outputs{};
        for (unsigned i = 0; i < site.outputs; ++i) {
            // The deepest output cell goes to the first register.
            Scalar cell;
            if (!memory.resolve(s.data.pointer + Cell(site.outputs - 1 - i) * 8, 8, false, false, cell))
                return python_resumes();
            outputs[i] = cell.read(8);
        }
        const Cell after_pop = s.data.pointer + Cell(site.outputs) * 8;
        if (after_pop - s.data.floor < Cell(routine.outputs) * 8) return python_resumes();
        std::array<Scalar, 8> destinations;
        for (unsigned i = 0; i < routine.outputs; ++i) {
            if (!memory.resolve(after_pop - Cell(i + 1) * 8, 8, true, false, destinations[i]))
                return python_resumes();
        }
        std::array<uint64_t, 8> results{};
        megapad::hybrid::RoutineCallbackStop callback{};
        uint64_t instructions = 0;
        const int outcome = routine_call_->resume(routine_call_->runner, frame.slot,
            outputs.data(), site.outputs, s.machine_allowance, results.data(), routine.outputs,
            &callback, &instructions);
        if (outcome == megapad::hybrid::ROUTINE_DECLINED) return python_resumes();
        account_machine(s, instructions);
        s.data.pointer = after_pop;
        frame.in_callback = false;
        if (outcome == megapad::hybrid::ROUTINE_RETURNED) {
            for (unsigned i = 0; i < routine.outputs; ++i)
                destinations[i].write(results[i], 8);
            s.data.pointer = after_pop - Cell(routine.outputs) * 8;
            s.returns.pointer = frame.frontier;
            s.xt = frame.caller_xt;
            s.ip = frame.caller_ip;
            const auto caller = plans_.find(s.xt);
            plan = caller == plans_.end() ? nullptr : &caller->second;
            s.frames.pop_back();
            return true;
        }
        if (outcome != megapad::hybrid::ROUTINE_CALLBACK) {
            s.machine_handoff = MACHINE_STOPPED_RESUME;
            return true;
        }
        return enter_callback(callback, memory, s, continuations, plan);
    }

    enum SlotKind { USER_CELL, CONTINUATION, PYTHON_BOUNDARY };

    [[gnu::noinline]] SlotKind return_slot(Cell slot, Cell raw, RunState& s,
                                           const py::dict& continuations,
                                           ContinuationUpdate& value) {
        // Only a return into it may consume a callback's machine return slot.
        if (!s.frames.empty() && s.frames.back().in_callback && slot == s.frames.back().slot)
            return PYTHON_BOUNDARY;
        if (const auto* changed = s.changed.find(slot)) {
            value = *changed;
            if (value.caller == 0) return USER_CELL; // >R erased the old type.
            return value.raw == raw ? CONTINUATION : PYTHON_BOUNDARY;
        }
        py::int_ key(slot);
        PyObject* entry = PyDict_GetItemWithError(continuations.ptr(), key.ptr());
        if (entry == nullptr) {
            if (PyErr_Occurred()) throw py::error_already_set();
            // The dictionary does not change during a run, so the answer
            // holds until this run retypes the cell. Recording it spares the
            // next look at this cell the dictionary and lets the direct path
            // read the cell. Exported, the record erases a type that was
            // not there.
            s.changed.set(slot, ContinuationUpdate{});
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
            !continuation.attr("fault_abort").is_none())
            return PYTHON_BOUNDARY;
        value = ContinuationUpdate{continuation.attr("xt").cast<Cell>(),
                                   continuation.attr("ip").cast<Cell>(), raw};
        return CONTINUATION;
    }

    // Keep byte-span allocation and cleanup outside the scalar dispatch loop.
    [[gnu::noinline]] static bool execute_bulk(uint32_t opcode, MemoryRun& memory,
                                               RunState& s, StackOperation& stack) {
        auto& v = stack.values;
        auto& out = stack.result;
        unsigned produced = 0;
        switch (opcode) {
        case OP_COMPARE: {
            if (!stack.inputs(4)) return false;
            ByteSpan left, right;
            const Cell length = std::min(v[2], v[0]);
            // Validate both complete prefixes even when their first bytes
            // differ; reference block reads must still report later faults.
            if (!memory.resolve_bytes(v[3], length, false, left) ||
                !memory.resolve_bytes(v[1], length, false, right)) return false;
            const int order = left.compare(right);
            out[0] = order < 0 ? MASK : order > 0 ? 1 :
                v[2] < v[0] ? MASK : v[2] > v[0] ? 1 : 0;
            produced = 1;
            break;
        }
        case OP_FILL: {
            if (!stack.inputs(3) || !stack.outputs(0)) return false;
            ByteSpan destination;
            const uint8_t value = static_cast<uint8_t>(v[0]);
            // Zero fills deliberately leave absent sparse pages unallocated.
            if (!memory.resolve_bytes(v[2], v[1], value != 0, destination)) return false;
            stack.commit();
            destination.fill(value);
            ++s.ip;
            return true;
        }
        case OP_CMOVE: case OP_CMOVE_UP: case OP_MOVE: {
            if (!stack.inputs(3) || !stack.outputs(0)) return false;
            const Cell length = v[0], destination = v[1], source = v[2];
            if (length != 0 && !(opcode == OP_MOVE && source == destination)) {
                std::vector<uint8_t> payload;
                if (length > payload.max_size()) return false;
                ByteSpan from, to;
                if (!memory.resolve_bytes(source, length, false, from) ||
                    !memory.resolve_bytes(destination, length, true, to)) return false;
                payload.resize(static_cast<size_t>(length));
                // Reference ordinary copies read before writing, even when
                // host-installed page objects happen to alias each other.
                from.read_into(payload.data());
                if (opcode == OP_CMOVE && source < destination &&
                    destination - source < length) {
                    const Cell stride = destination - source;
                    for (Cell offset = stride; offset < length;) {
                        const Cell size = std::min(stride, length - offset);
                        std::memcpy(payload.data() + offset, payload.data(),
                                    static_cast<size_t>(size));
                        offset += size;
                    }
                } else if (opcode == OP_CMOVE_UP && destination < source &&
                           source - destination < length) {
                    const Cell stride = source - destination;
                    for (Cell end = length - stride; end != 0;) {
                        const Cell size = std::min(stride, end);
                        std::memcpy(payload.data() + end - size,
                                    payload.data() + length - size,
                                    static_cast<size_t>(size));
                        end -= size;
                    }
                }
                stack.commit();
                to.write_from(payload.data());
            } else {
                // MOVE also treats identical (even unmapped) addresses as a
                // no-op. CMOVE/CMOVE> must still validate a nonempty self-copy.
                stack.commit();
            }
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

    bool tile_admitted(RunState& s) {
        if (!s.tile) return false;
        if (!s.tile_checked) {
            s.tile_checked = true;
            s.tile_allowed = !tile_guard_ || tile_guard_().cast<bool>();
        }
        return s.tile_allowed;
    }

    // The general way to the machine's state. Every cell is resolved through
    // the page tables, a return cell's kind comes from this run's record or
    // from Python's dictionary, and a call site's target is bound here. It
    // performs every operation the program knows.
    struct GeneralPath {
        static constexpr bool complete = true;
        using Memory = Scalar;

        GeneralPath(NativeProgram& program, MemoryRun& memory, RunState& state,
                    const py::dict& continuations, Plan*& plan) noexcept
            : program(program), memory(memory), s(state), continuations(continuations),
              plan(plan), ip(state.ip), xt(state.xt), cookie(state.cookie),
              steps(state.steps) {}

        StackOperation data() noexcept { return StackOperation(memory, s.data); }
        StackOperation returns() noexcept { return StackOperation(memory, s.returns); }
        // Store an accessor's outputs; its stack's pointer follows.
        void commit(StackOperation& cells) noexcept { cells.commit(); }
        // Move a stack's pointer past ``count`` cells, which keep their bytes.
        void release(StackOperation& cells, unsigned count) noexcept { cells.release(count); }
        Cell sp() const noexcept { return s.data.pointer; }
        Cell rp() const noexcept { return s.returns.pointer; }
        // The cost of the operation about to run, for a callback inside it.
        void begin(Cell own) noexcept { s.operation_cost = own; }
        void count(Cell taken) noexcept { steps += taken; }
        bool ordinary(Cell address, unsigned width, bool write, Memory& cell) {
            return memory.resolve(address, width, write, true, cell);
        }
        SlotKind kind(Cell slot, Cell raw, ContinuationUpdate& value) {
            return program.return_slot(slot, raw, s, continuations, value);
        }
        // The plan a call enters, bound to its site at the first call. An
        // overlaid word is entered only by Python.
        Plan* callee(Instruction& operation, Cell target_xt) {
            Plan* target = operation.target;
            if (target == nullptr || operation.a != target_xt) {
                const auto found = program.plans_.find(target_xt);
                if (found == program.plans_.end() || program.overlaid_.count(target_xt) != 0)
                    return nullptr;
                target = &found->second;
                operation.a = target_xt;
                operation.target = target;
            }
            return target;
        }
        Plan* caller(const ContinuationUpdate& continuation) {
            if (continuation.plan != nullptr) return continuation.plan;
            const auto found = program.plans_.find(continuation.caller);
            return found == program.plans_.end() ? nullptr : &found->second;
        }
        void enter(Plan* target) noexcept { plan = target; }
        // The defining word's plan, found here at a DOES> entry's first call.
        Plan* does_plan(DoesEntry& entry) {
            if (entry.plan == nullptr) {
                const auto found = program.plans_.find(entry.xt);
                if (found == program.plans_.end()) return nullptr;
                entry.plan = &found->second;
            }
            return entry.plan;
        }

        NativeProgram& program;
        MemoryRun& memory;
        RunState& s;
        const py::dict& continuations;
        Plan*& plan;
        Cell& ip;
        Cell& xt;
        Cell& cookie;
        Cell& steps;
    };

#ifdef MEGAFORTH_DIRECT_PATH
    // The direct way, for what a run has already proved. The instruction
    // pointer, both stack pointers and the step count are held here, stack
    // cells are addressed inside the page qualified for each stack, ordinary
    // memory inside the hot page, a return cell has the kind this run gave
    // it, and a call enters the plan already bound to its site. Whatever it
    // cannot prove from these it declines, and the general path decides.
    struct DirectPath {
        static constexpr bool complete = false;
        using Memory = DirectCell;

        DirectPath(RunState& state, const MemoryRun& memory, Plan* plan) noexcept
            : s(state), plan(plan), code(plan->data()), size(plan->size()),
              ip(state.ip), xt(state.xt), cookie(state.cookie), steps(state.steps),
              memory_(memory),
              data_window_(memory.stack_window(false, state.data.floor, state.data.empty)),
              return_window_(
                  memory.stack_window(true, state.returns.floor, state.returns.empty)),
              data_pointer_(state.data.pointer), return_pointer_(state.returns.pointer) {}

        // Give the run's state what this path has completed.
        void publish() noexcept {
            s.ip = ip;
            s.xt = xt;
            s.cookie = cookie;
            s.steps = steps;
            s.data.pointer = data_pointer_;
            s.returns.pointer = return_pointer_;
        }

        DataWindow data() const noexcept { return DataWindow(data_window_, data_pointer_); }
        ReturnWindow returns() const noexcept {
            return ReturnWindow(return_window_, return_pointer_);
        }
        void commit(DataWindow& cells) noexcept { data_pointer_ = cells.apply(); }
        void commit(ReturnWindow& cells) noexcept { return_pointer_ = cells.apply(); }
        void release(DataWindow& cells, unsigned count) noexcept {
            data_pointer_ = cells.pass(count);
        }
        void release(ReturnWindow& cells, unsigned count) noexcept {
            return_pointer_ = cells.pass(count);
        }
        Cell sp() const noexcept { return data_pointer_; }
        Cell rp() const noexcept { return return_pointer_; }
        void begin(Cell) noexcept {}
        void count(Cell taken) noexcept { steps += taken; }
        bool ordinary(Cell address, unsigned width, bool, Memory& cell) const noexcept {
            cell.bytes = memory_.ordinary_span(address, width);
            return cell.bytes != nullptr;
        }
        // A cell this run has not typed is the general path's to look up.
        SlotKind kind(Cell slot, Cell raw, ContinuationUpdate& value) const noexcept {
            const ContinuationUpdate* known = s.changed.find(slot);
            if (known == nullptr) return PYTHON_BOUNDARY;
            value = *known;
            if (value.caller == 0) return USER_CELL;
            return value.raw == raw ? CONTINUATION : PYTHON_BOUNDARY;
        }
        Plan* callee(const Instruction& operation, Cell target_xt) const noexcept {
            return operation.a == target_xt ? operation.target : nullptr;
        }
        Plan* caller(const ContinuationUpdate& continuation) const noexcept {
            return continuation.plan;
        }
        void enter(Plan* target) noexcept {
            plan = target;
            code = target->data();
            size = target->size();
        }
        Plan* does_plan(const DoesEntry& entry) const noexcept { return entry.plan; }

        RunState& s;
        Plan* plan;
        Instruction* code;
        Cell size;
        Cell ip;
        Cell xt;
        Cell cookie;
        Cell steps;

    private:
        const MemoryRun& memory_;
        const MemoryRun::StackWindow data_window_;
        const MemoryRun::StackWindow return_window_;
        Cell data_pointer_;
        Cell return_pointer_;
    };
#endif

    // The top ``count`` return cells an accessor has read are all the user's.
    // Like the accessors, it names each cell by a constant.
    template <class Path, class Stack>
    static bool user_cells(Path& path, const Stack& returns, unsigned count) {
        const auto user = [&](unsigned index) -> bool {
            ContinuationUpdate continuation;
            return path.kind(path.rp() + Cell{8} * index, returns.values[index],
                             continuation) == USER_CELL;
        };
        switch (count) {
        case 4: if (!user(3)) return false; [[fallthrough]];
        case 3: if (!user(2)) return false; [[fallthrough]];
        case 2: if (!user(1)) return false; [[fallthrough]];
        case 1: if (!user(0)) return false; [[fallthrough]];
        default: return true;
        }
    }

    // One operation's effect, written once. ``Path`` says how the machine's
    // state is reached. Either the whole effect happens and the instruction
    // pointer moves, or nothing changes and the answer is false. The direct
    // path then leaves the operation to the general one, and the general
    // path leaves it to Python, which performs it from the same boundary.
    // Every precondition is therefore proved before the first change, and a
    // completed effect has counted the operation's steps.
    //
    // An accessor is asked for a count of cells that is constant where it is
    // written, and the path, not the accessor, moves a stack's pointer. Both
    // let the direct path keep the cells and the pointers in host registers.
    // The helpers below are inlined into every operation that uses them.
    template <class Path>
    bool effect(Path& path, Instruction& operation) {
        const auto opcode = operation.opcode;
        auto stack = path.data();
        auto& v = stack.values;
        auto& out = stack.result;
        typename Path::Memory cell;

        // A completed effect counts its steps last. The opcode is known in
        // each case below, so the direct path adds a constant.
        const auto done = [&]() __attribute__((always_inline)) -> bool {
            path.count(cost(opcode));
            return true;
        };
        // The usual ending: the operation leaves ``produced`` cells in place
        // of the ones it read. Outputs are bottom-to-top.
        const auto leave = [&](unsigned produced) __attribute__((always_inline)) -> bool {
            if (!stack.outputs(produced)) return false;
            path.commit(stack);
            ++path.ip;
            return done();
        };
        const auto unary = [&](uint32_t which) __attribute__((always_inline)) -> bool {
            return stack.inputs(1) && unary_value(which, v[0], out[0]) && leave(1);
        };
        const auto binary = [&](uint32_t which) __attribute__((always_inline)) -> bool {
            return stack.inputs(2) && binary_value(which, v[1], v[0], out[0]) && leave(1);
        };
        const auto fetch = [&](unsigned width) __attribute__((always_inline)) -> bool {
            if (!stack.inputs(1) || !path.ordinary(v[0], width, false, cell)) return false;
            out[0] = cell.read(width);
            return leave(1);
        };
        // I and J: the index of the loop whose two cells end ``count`` cells
        // down, all of them the user's.
        const auto loop_index = [&](unsigned count) __attribute__((always_inline)) -> bool {
            auto returns = path.returns();
            if (!stack.inputs(0) || !returns.inputs(count) ||
                !user_cells(path, returns, count)) return false;
            out[0] = returns.values[count - 2];
            return leave(1);
        };
        // A store to ordinary memory. The address is on top, and ``value``
        // gives what is written once the cell is found.
        const auto store = [&](unsigned taken, unsigned width, auto&& value)
                __attribute__((always_inline)) -> bool {
            if (!stack.inputs(taken) || !stack.outputs(0) ||
                !path.ordinary(v[0], width, true, cell)) return false;
            const Cell written = value();
            path.commit(stack);
            cell.write(written, width);
            ++path.ip;
            return done();
        };

        switch (opcode) {
        case OP_BRANCH:
            path.ip = operation.a;
            return done();
        case OP_BRANCH_ZERO:
            if (!stack.inputs(1) || !stack.outputs(0)) return false;
            path.commit(stack);
            path.ip = v[0] == 0 ? operation.a : path.ip + 1;
            return done();
        case OP_ABORT_IF:
            // A true flag aborts, which Python performs from this boundary.
            if (!stack.inputs(1) || !stack.outputs(0) || v[0] != 0) return false;
            path.commit(stack);
            ++path.ip;
            return done();
        case OP_CALL: case OP_EXECUTE: {
            Cell target_xt = operation.a;
            if (opcode == OP_EXECUTE) {
                if (!stack.inputs(1) || !stack.outputs(0)) return false;
                target_xt = v[0];
            }
            Plan* target = path.callee(operation, target_xt);
            auto returns = path.returns();
            if (target == nullptr || target->empty() ||
                !returns.inputs(0) || !returns.outputs(1)) return false;
            Cell cookie = path.cookie;
            Cell raw;
            if (!next_continuation_cookie(cookie, path.xt, raw)) return false;
            // Allocate the retained side record before committing any effect.
            path.s.changed.set(path.rp() - 8,
                ContinuationUpdate{path.xt, path.ip + 1, raw, path.plan});
            if (opcode == OP_EXECUTE) path.commit(stack);
            returns.result[0] = raw;
            path.commit(returns);
            path.cookie = cookie;
            path.xt = target_xt;
            path.ip = 0;
            path.enter(target);
            return done();
        }
        case OP_DOES: {
            // A CREATE ... DOES> child pushes its body address and calls the
            // defining word's code after DOES>. Like a constant's call it
            // costs the call's tick and its own, and both pushes are
            // preflighted, so a failure leaves Python the exact fault.
            Plan* target = path.does_plan(*operation.does);
            auto returns = path.returns();
            if (target == nullptr || !stack.inputs(0) || !stack.outputs(1) ||
                !returns.inputs(0) || !returns.outputs(1)) return false;
            Cell cookie = path.cookie;
            Cell raw;
            if (!next_continuation_cookie(cookie, path.xt, raw)) return false;
            path.s.changed.set(path.rp() - 8,
                ContinuationUpdate{path.xt, path.ip + 1, raw, path.plan});
            out[0] = operation.a;
            path.commit(stack);
            returns.result[0] = raw;
            path.commit(returns);
            path.cookie = cookie;
            path.xt = operation.does->xt;
            path.ip = operation.does->ip;
            path.enter(target);
            return done();
        }
        case OP_RETURN: {
            if constexpr (Path::complete) {
                RunState& s = path.s;
                if (!s.frames.empty() && s.frames.back().in_callback &&
                        s.returns.pointer == s.frames.back().slot) {
                    // A changed slot no longer holds this machine return.
                    Scalar slot;
                    if (!path.memory.resolve(s.returns.pointer, 8, false, false, slot) ||
                            slot.read(8) != s.frames.back().raw)
                        return false;
                    return return_into_machine(path.memory, s, path.continuations,
                                               path.plan, false) && done();
                }
            }
            auto returns = path.returns();
            ContinuationUpdate continuation;
            if (!returns.inputs(1) ||
                path.kind(path.rp(), returns.values[0], continuation) != CONTINUATION)
                return false;
            Plan* caller = path.caller(continuation);
            if (caller == nullptr) return false;
            path.release(returns, 1);
            path.xt = continuation.caller;
            path.ip = continuation.ip;
            path.enter(caller);
            return done();
        }
        case OP_R_PUSH: {
            auto returns = path.returns();
            if (!stack.inputs(1) || !stack.outputs(0) ||
                !returns.inputs(0) || !returns.outputs(1)) return false;
            // A user push deletes type metadata even when its value happens to
            // equal the old cookie. Retain that deletion across native exits.
            path.s.changed.set(path.rp() - 8, ContinuationUpdate{});
            returns.result[0] = v[0];
            path.commit(returns);
            path.commit(stack);
            ++path.ip;
            return done();
        }
        case OP_R_POP: case OP_R_PEEK: {
            auto returns = path.returns();
            if (!stack.inputs(0) || !stack.outputs(1) || !returns.inputs(1) ||
                !user_cells(path, returns, 1)) return false;
            out[0] = returns.values[0];
            if (opcode == OP_R_POP) path.release(returns, 1);
            path.commit(stack);
            ++path.ip;
            return done();
        }
        case OP_DO: case OP_QUESTION_DO: case OP_R_PUSH_PAIR: {
            // DO and 2>R put the deeper data cell deeper on the return stack:
            // a loop's limit lies below its index. Both stacks are preflighted
            // before either changes, so a failure leaves Python the exact
            // reference fault.
            if (!stack.inputs(2) || !stack.outputs(0)) return false;
            if (opcode == OP_QUESTION_DO && v[0] == v[1]) {
                path.commit(stack);
                path.ip = operation.a;
                return done();
            }
            auto returns = path.returns();
            if (!returns.inputs(0) || !returns.outputs(2)) return false;
            returns.result[0] = v[1];
            returns.result[1] = v[0];
            path.s.changed.erase_pair(path.rp() - 16, path.rp() - 8);
            path.commit(returns);
            path.commit(stack);
            ++path.ip;
            return done();
        }
        case OP_R_POP_PAIR: case OP_R_PEEK_PAIR: {
            // 2R> and 2R@ take two user cells, never a continuation or a
            // machine return, and push the deeper one first.
            auto returns = path.returns();
            if (!stack.inputs(0) || !stack.outputs(2) || !returns.inputs(2) ||
                !user_cells(path, returns, 2)) return false;
            out[0] = returns.values[1];
            out[1] = returns.values[0];
            if (opcode == OP_R_POP_PAIR) path.release(returns, 2);
            path.commit(stack);
            ++path.ip;
            return done();
        }
        case OP_LOOP: case OP_PLUS_LOOP: case OP_UNLOOP: {
            const bool plus = opcode == OP_PLUS_LOOP;
            auto returns = path.returns();
            if (!(plus ? stack.inputs(1) : stack.inputs(0)) || !stack.outputs(0) ||
                !returns.inputs(2) || !user_cells(path, returns, 2)) return false;
            const Cell next = returns.values[0] + (plus ? v[0] : Cell{1});
            if (opcode != OP_UNLOOP && next != returns.values[1]) {
                // Only the index is written, so only its cell must take one.
                auto index = path.returns();
                if (!index.inputs(1) || !index.outputs(1)) return false;
                path.commit(stack);
                index.result[0] = next;
                path.commit(index);
                path.ip = operation.a;
                return done();
            }
            path.commit(stack);
            path.release(returns, 2); // retain the last index bytes
            ++path.ip;
            return done();
        }
        case OP_I: return loop_index(2);
        case OP_J: return loop_index(4);
        case OP_LITERAL: case OP_PUSH_CELL:
            if (!stack.inputs(0)) return false;
            out[0] = operation.a;
            return leave(1);
        case OP_STRING_LITERAL:
            if (!stack.inputs(0)) return false;
            out[0] = operation.a;
            out[1] = operation.b;
            return leave(2);
        case OP_TRUE: case OP_FALSE:
            if (!stack.inputs(0)) return false;
            out[0] = opcode == OP_TRUE ? MASK : 0;
            return leave(1);
        case OP_SP_FETCH:
            if (!stack.inputs(0)) return false;
            out[0] = path.sp();
            return leave(1);
        case OP_RP_FETCH:
            if (!stack.inputs(0)) return false;
            out[0] = path.rp();
            if (!leave(1)) return false;
            // Export captures before Python observes a fault or suspension. A
            // failed push takes the reference path, which registers RP@ before
            // raising its ordinary overflow fault.
            ++path.s.pointer_captures;
            return true;
        case OP_FPCSR_FETCH:
            if (!stack.inputs(0)) return false;
            out[0] = path.s.fpcsr;
            return leave(1);
        case OP_FPCSR_STORE:
            if (!stack.inputs(1) || !stack.outputs(0)) return false;
            path.commit(stack);
            path.s.fpcsr = v[0] & 0x1f7;
            ++path.ip;
            return done();
        case OP_SCALAR_FP: {
            if (operation.a > 0xff || operation.b < 1 || operation.b > 3)
                return false;
            const auto fc = static_cast<unsigned>(operation.a);
            const auto arity = static_cast<unsigned>(operation.b);
            // FCMP has no cell result and is not a hosted BIOS scalar word.
            // Decline all malformed descriptors and reserved rounding modes
            // before any stack/flag effects. Python retains its partial pops
            // and instruction-fault flow at this original call boundary.
            const auto code = fc & 0x3f;
            const unsigned expected_arity =
                code <= 3 || code == 5 || code == 6 ||
                    (code >= 0x11 && code <= 0x13) ? 2 :
                code == 7 || code == 8 ? 3 :
                code == 4 || code == 0x14 || code >= 0x20 ? 1 : 0;
            if (arity != expected_arity ||
                megapad::scalar_fp::validate(fc, 0, path.s.fpcsr) != nullptr ||
                !(arity == 1 ? stack.inputs(1) : arity == 2 ? stack.inputs(2) :
                  stack.inputs(3)) || !stack.outputs(1))
                return false;
            const Cell rd = arity == 2 ? v[1] : v[0];
            const Cell rs = arity == 3 ? v[2] : v[0];
            const Cell rt = arity == 3 ? v[1] : 0;
            const auto outcome = megapad::scalar_fp::execute(fc, rd, rs, rt, path.s.fpcsr);
            out[0] = outcome.value;
            path.commit(stack);
            path.s.fpcsr |= outcome.flags;
            ++path.ip;
            return done();
        }
        case OP_FETCH_VALUE:
            if (!stack.inputs(0) || !path.ordinary(operation.a, 8, false, cell)) return false;
            out[0] = cell.read(8);
            return leave(1);
        case OP_STORE_VALUE:
            if (!stack.inputs(1) || !stack.outputs(0) ||
                !path.ordinary(operation.a, 8, true, cell)) return false;
            path.commit(stack);
            cell.write(v[0], 8);
            ++path.ip;
            return done();
        case OP_DUP:
            if (!stack.inputs(1)) return false;
            out[0] = v[0];
            out[1] = v[0];
            return leave(2);
        case OP_QUESTION_DUP:
            if (!stack.inputs(1)) return false;
            out[0] = v[0];
            out[1] = v[0];
            return v[0] != 0 ? leave(2) : leave(1);
        case OP_DROP:
            return stack.inputs(1) && leave(0);
        case OP_TWO_DROP:
            return stack.inputs(2) && leave(0);
        case OP_SWAP:
            if (!stack.inputs(2)) return false;
            out[0] = v[0];
            out[1] = v[1];
            return leave(2);
        case OP_OVER:
            if (!stack.inputs(2)) return false;
            out[0] = v[1];
            out[1] = v[0];
            out[2] = v[1];
            return leave(3);
        case OP_NIP:
            if (!stack.inputs(2)) return false;
            out[0] = v[0];
            return leave(1);
        case OP_TUCK:
            if (!stack.inputs(2)) return false;
            out[0] = v[0];
            out[1] = v[1];
            out[2] = v[0];
            return leave(3);
        case OP_ROT:
            if (!stack.inputs(3)) return false;
            out[0] = v[1];
            out[1] = v[0];
            out[2] = v[2];
            return leave(3);
        case OP_MINUS_ROT:
            if (!stack.inputs(3)) return false;
            out[0] = v[0];
            out[1] = v[2];
            out[2] = v[1];
            return leave(3);
        case OP_TWO_DUP:
            if (!stack.inputs(2)) return false;
            out[0] = v[1];
            out[1] = v[0];
            out[2] = v[1];
            out[3] = v[0];
            return leave(4);
        case OP_TWO_OVER:
            if (!stack.inputs(4)) return false;
            out[0] = v[3];
            out[1] = v[2];
            out[2] = v[1];
            out[3] = v[0];
            out[4] = v[3];
            out[5] = v[2];
            return leave(6);
        case OP_TWO_SWAP:
            if (!stack.inputs(4)) return false;
            out[0] = v[1];
            out[1] = v[0];
            out[2] = v[3];
            out[3] = v[2];
            return leave(4);
        case OP_PICK:
            if (!stack.inputs(1) || stack.depth() < 2 || v[0] > stack.depth() - 2 ||
                !stack.deep(v[0] + 1, out[0])) return false;
            return leave(1);
        case OP_NEGATE: return unary(OP_NEGATE);
        case OP_ABS: return unary(OP_ABS);
        case OP_ONE_PLUS: return unary(OP_ONE_PLUS);
        case OP_ONE_MINUS: return unary(OP_ONE_MINUS);
        case OP_TWO_MULTIPLY: return unary(OP_TWO_MULTIPLY);
        case OP_TWO_DIVIDE: return unary(OP_TWO_DIVIDE);
        case OP_CELLS: return unary(OP_CELLS);
        case OP_CELL_PLUS: return unary(OP_CELL_PLUS);
        case OP_INVERT: return unary(OP_INVERT);
        case OP_ZERO_EQUAL: return unary(OP_ZERO_EQUAL);
        case OP_ZERO_NOT_EQUAL: return unary(OP_ZERO_NOT_EQUAL);
        case OP_ZERO_LESS: return unary(OP_ZERO_LESS);
        case OP_ZERO_GREATER: return unary(OP_ZERO_GREATER);
        case OP_BSWAP: return unary(OP_BSWAP);
        case OP_ADD: return binary(OP_ADD);
        case OP_SUBTRACT: return binary(OP_SUBTRACT);
        case OP_MULTIPLY: return binary(OP_MULTIPLY);
        case OP_MIN: return binary(OP_MIN);
        case OP_MAX: return binary(OP_MAX);
        case OP_AND: return binary(OP_AND);
        case OP_OR: return binary(OP_OR);
        case OP_XOR: return binary(OP_XOR);
        case OP_LSHIFT: return binary(OP_LSHIFT);
        case OP_RSHIFT: return binary(OP_RSHIFT);
        case OP_EQUAL: return binary(OP_EQUAL);
        case OP_NOT_EQUAL: return binary(OP_NOT_EQUAL);
        case OP_ULESS: return binary(OP_ULESS);
        case OP_UGREATER: return binary(OP_UGREATER);
        case OP_LESS: return binary(OP_LESS);
        case OP_LESS_EQUAL: return binary(OP_LESS_EQUAL);
        case OP_GREATER: return binary(OP_GREATER);
        case OP_GREATER_EQUAL: return binary(OP_GREATER_EQUAL);
        case OP_UMULTIPLY: {
            if (!stack.inputs(2)) return false;
            const __uint128_t product = static_cast<__uint128_t>(v[1]) * v[0];
            out[0] = static_cast<Cell>(product);
            out[1] = static_cast<Cell>(product >> 64);
            return leave(2);
        }
        case OP_DIVIDE: case OP_MODULO: case OP_DIVMOD: {
            // Division by zero and the one overflowing quotient fault, which
            // Python raises.
            if (!stack.inputs(2) || v[0] == 0 ||
                (opcode != OP_MODULO && v[1] == SIGN && v[0] == MASK)) return false;
            Cell quotient = magnitude(v[1]) / magnitude(v[0]);
            if ((v[1] ^ v[0]) & SIGN) quotient = Cell{0} - quotient;
            const Cell remainder = v[1] - quotient * v[0];
            out[0] = opcode == OP_DIVIDE ? quotient : remainder;
            out[1] = quotient;
            return opcode == OP_DIVMOD ? leave(2) : leave(1);
        }
        case OP_WITHIN:
            if (!stack.inputs(3)) return false;
            out[0] = flag(v[2] - v[1] < v[0] - v[1]);
            return leave(1);
        case OP_FETCH: return fetch(8);
        case OP_C_FETCH: return fetch(1);
        case OP_W_FETCH: return fetch(2);
        case OP_L_FETCH: return fetch(4);
        case OP_COUNT:
            if (!stack.inputs(1) || !path.ordinary(v[0], 1, false, cell)) return false;
            out[0] = v[0] + 1;
            out[1] = cell.read(1);
            return leave(2);
        case OP_STORE: return store(2, 8, [&] { return v[1]; });
        case OP_C_STORE: return store(2, 1, [&] { return v[1]; });
        case OP_W_STORE: return store(2, 2, [&] { return v[1]; });
        case OP_L_STORE: return store(2, 4, [&] { return v[1]; });
        case OP_PLUS_STORE: return store(2, 8, [&] { return cell.read(8) + v[1]; });
        case OP_OFF: return store(1, 8, [] { return Cell{0}; });
        case OP_ON: return store(1, 8, [] { return MASK; });
        default:
            if constexpr (Path::complete) return rare_effect(path, operation, stack) && done();
            else return false;
        }
    }

    // The operations only the general path performs: they resolve byte
    // spans, ask the tile service's owner, or enter a machine routine.
    [[gnu::noinline]] bool rare_effect(GeneralPath& path, Instruction& operation,
                                       StackOperation& stack) {
        const auto opcode = operation.opcode;
        RunState& s = path.s;
        auto& v = stack.values;
        switch (opcode) {
        case OP_TILE_MODE_SET:
            if (!tile_admitted(s) || !stack.inputs(0) || !stack.outputs(0)) return false;
            stack.commit();
            s.tile_mode = operation.a & 0x7F;
            ++s.ip;
            return true;
        case OP_TILE_MODE_STORE: case OP_TILE_SOURCE0_STORE: case OP_TILE_SOURCE1_STORE:
        case OP_TILE_DESTINATION_STORE:
            if (!tile_admitted(s) || !stack.inputs(1) || !stack.outputs(0)) return false;
            stack.commit();
            if (opcode == OP_TILE_MODE_STORE) s.tile_mode = v[0] & 0x7F;
            else if (opcode == OP_TILE_SOURCE0_STORE) s.tile_source0 = v[0];
            else if (opcode == OP_TILE_SOURCE1_STORE) s.tile_source1 = v[0];
            else s.tile_destination = v[0];
            ++s.ip;
            return true;
        case OP_TILE_BINARY:
            return tile_admitted(s) && execute_tile_binary(operation, path.memory, s, stack);
        case OP_COMPARE: case OP_FILL: case OP_CMOVE: case OP_CMOVE_UP: case OP_MOVE:
            return execute_bulk(opcode, path.memory, s, stack);
        case OP_CALL_ROUTINE:
            return call_routine(operation, path.memory, s, path.continuations, path.plan);
        default:
            return false;
        }
    }

    // A pair the planner fused, done as one when both halves can finish. A
    // false answer leaves the first half to run alone.
    template <class Path>
    bool fused_effect(Path& path, const Instruction& first, const Instruction& next) {
        auto stack = path.data();
        const Cell pair = cost(first.opcode) + cost(first.fused);
        if (first.fused >= OP_FETCH && first.fused <= OP_L_FETCH) {
            const unsigned width = first.fused == OP_FETCH ? 8 :
                1U << (first.fused - OP_C_FETCH);
            typename Path::Memory source;
            if (!stack.inputs(0) || !stack.outputs(1) ||
                !path.ordinary(first.a, width, false, source))
                return false;
            // A guest read can alias the address just pushed onto its stack.
            // Perform that write before reading, after both operations have
            // passed preflight. A miss leaves the original first op intact.
            stack.result[0] = first.a;
            path.commit(stack);
            stack.result[0] = source.read(width);
            path.commit(stack);
            path.ip += 2;
            path.count(pair);
            return true;
        }
        if (!stack.inputs(1)) return false;
        const Cell left = stack.values[0];
        if (first.opcode == OP_DUP) {
            if (!stack.outputs(2)) return false;
            stack.result[0] = left;
            stack.result[1] = left;
            path.commit(stack);
            path.release(stack, 1); // BRANCH_ZERO consumes the duplicate.
            path.ip = left == 0 ? next.a : path.ip + 2;
            path.count(pair);
            return true;
        }
        // Even though the literal is consumed immediately, its guest stack
        // slot must exist and retain its bytes below the result.
        Cell value;
        if (!stack.outputs(2) || !binary_value(first.fused, left, first.a, value))
            return false;
        stack.result[0] = value;
        stack.result[1] = first.a;
        path.commit(stack);
        path.release(stack, 1);
        path.ip += 2;
        path.count(pair);
        return true;
    }

    // Run ``operation``, the one at the instruction pointer, or the pair it
    // heads. False leaves everything as it was: the operation is one this
    // program stops at, or the allowance does not cover it, or the path
    // cannot perform it.
    template <class Path>
    bool step(Path& path, Instruction& operation, Cell limit) {
        const Cell left = limit - path.steps;
        const Cell own = cost(operation.opcode);
        if (operation.opcode == OP_STOP || own > left) return false;
        if (operation.fused != OP_STOP && own + cost(operation.fused) <= left &&
            fused_effect(path, operation, *(&operation + 1)))
            return true;
        path.begin(own);
        return effect(path, operation);
    }

#ifdef MEGAFORTH_DIRECT_PATH
    // Run on the direct path until an operation needs the general one, the
    // plan ends or the step allowance is spent. ``state`` holds the completed
    // prefix on every way out, an allocation failure in the change record
    // included. Returns the plan execution stands in.
    //
    // Flattening inlines the path's accessors into every operation. One left
    // out of line would force the values it reaches into memory.
    [[gnu::flatten]] Plan* run_direct(RunState& state, const MemoryRun& memory, Plan* plan,
                                      const Cell limit) {
        DirectPath path(state, memory, plan);
        // No operation costs more than a primitive's steps, so no fused pair
        // costs more than twice that. With that many steps left, step's
        // checks of the allowance cannot fail and the compiler drops them.
        // The general path runs the last few steps of an interval.
        constexpr Cell WIDEST = 2 * cost(OP_DUP);
        try {
            while (path.ip < path.size && limit - path.steps >= WIDEST &&
                   step(path, path.code[path.ip], limit)) {}
        } catch (...) {
            path.publish();
            throw;
        }
        path.publish();
        return path.plan;
    }
#endif

    Cell page_size_;
    Cell direct_steps_ = 0;
    py::object continuation_type_;
    std::vector<Region> regions_;
    std::unordered_map<Cell, Plan> plans_;
    std::unordered_set<Cell> overlaid_;
    py::object tile_guard_;
    const megapad::hybrid::RoutineCall* routine_call_ = nullptr;
    py::object routine_entry_;
    std::vector<NativeRoutine> routines_;
    std::deque<DoesEntry> does_;
};
}  // namespace

PYBIND11_MODULE(_megaforth_native, module) {
    megapad::scalar_fp::register_bindings(module);
    megapad::keccak::register_bindings(module);
    megapad::tile_values::register_bindings(module);
    megapad::chiplet::register_bindings(module);
    megapad::dictionary::register_bindings(module);
    megapad::walker::register_bindings(module);
    module.attr("SEMANTIC_API_VERSION") = 11;
    module.attr("MACHINE_STOPPED_CALL") = py::int_(static_cast<uint32_t>(MACHINE_STOPPED_CALL));
    module.attr("MACHINE_STOPPED_RESUME") = py::int_(static_cast<uint32_t>(MACHINE_STOPPED_RESUME));
    module.attr("MACHINE_CALLBACK") = py::int_(static_cast<uint32_t>(MACHINE_CALLBACK));
    module.attr("MACHINE_RETURN") = py::int_(static_cast<uint32_t>(MACHINE_RETURN));
    module.doc() = "Native execution of generic hosted Forth semantic plans";
    py::class_<NativeProgram>(module, "NativeProgram",
                              megapad::collected::setup<NativeProgram>())
        .def(py::init<const py::iterable&, Cell, py::object, const py::iterable&>(),
             py::arg("regions"), py::arg("page_size"), py::arg("continuation_type"),
             py::arg("dense_regions") = py::tuple())
        .def("install", &NativeProgram::install, py::arg("xt"), py::arg("operations"),
             py::arg("overlaid") = false)
        .def("clear", &NativeProgram::clear)
        .def("does_entry", &NativeProgram::does_entry, py::arg("xt"), py::arg("ip"))
        .def_property_readonly("direct_steps", &NativeProgram::direct_steps)
        .def("bind_tile_guard", &NativeProgram::bind_tile_guard, py::arg("guard"))
        .def("snapshot_stack", &NativeProgram::snapshot_stack,
             py::arg("bounds"), py::arg("continuations") = py::none())
        .def("run", &NativeProgram::run, py::arg("xt"), py::arg("ip"),
             py::arg("data_state"), py::arg("return_state"),
             py::arg("continuations"), py::arg("remaining_steps"),
             py::arg("fpcsr") = 0, py::arg("machine_allowance") = 0,
             py::arg("tile_state") = py::none())
        .def("bind_routines", &NativeProgram::bind_routines, py::arg("entry"))
        .def("set_routine", &NativeProgram::set_routine, py::arg("index"), py::arg("image"),
             py::arg("inputs"), py::arg("outputs"), py::arg("rules"), py::arg("sites"))
        .def("set_routine_target", &NativeProgram::set_routine_target, py::arg("index"),
             py::arg("site"), py::arg("opcode"), py::arg("a"), py::arg("b"));
#define EXPORT_OPCODE(name) module.attr(#name) = py::int_(static_cast<uint32_t>(name))
    EXPORT_OPCODE(OP_STOP);
    EXPORT_OPCODE(OP_LITERAL); EXPORT_OPCODE(OP_BRANCH); EXPORT_OPCODE(OP_BRANCH_ZERO);
    EXPORT_OPCODE(OP_CALL); EXPORT_OPCODE(OP_RETURN); EXPORT_OPCODE(OP_STORE_VALUE);
    EXPORT_OPCODE(OP_STRING_LITERAL); EXPORT_OPCODE(OP_PUSH_CELL); EXPORT_OPCODE(OP_FETCH_VALUE);
    EXPORT_OPCODE(OP_R_PUSH); EXPORT_OPCODE(OP_R_POP); EXPORT_OPCODE(OP_R_PEEK);
    EXPORT_OPCODE(OP_DO); EXPORT_OPCODE(OP_QUESTION_DO); EXPORT_OPCODE(OP_LOOP);
    EXPORT_OPCODE(OP_PLUS_LOOP); EXPORT_OPCODE(OP_UNLOOP); EXPORT_OPCODE(OP_ABORT_IF);
    EXPORT_OPCODE(OP_R_PUSH_PAIR); EXPORT_OPCODE(OP_R_POP_PAIR); EXPORT_OPCODE(OP_R_PEEK_PAIR);
    EXPORT_OPCODE(OP_SCALAR_FP); EXPORT_OPCODE(OP_FPCSR_FETCH);
    EXPORT_OPCODE(OP_FPCSR_STORE); EXPORT_OPCODE(OP_CALL_ROUTINE);
    EXPORT_OPCODE(OP_TILE_MODE_SET); EXPORT_OPCODE(OP_TILE_MODE_STORE);
    EXPORT_OPCODE(OP_TILE_SOURCE0_STORE); EXPORT_OPCODE(OP_TILE_SOURCE1_STORE);
    EXPORT_OPCODE(OP_TILE_DESTINATION_STORE); EXPORT_OPCODE(OP_TILE_BINARY);
    EXPORT_OPCODE(OP_DOES);
    module.attr("TILE_ADD") = py::int_(static_cast<uint32_t>(TILE_ADD));
    module.attr("TILE_SUBTRACT") = py::int_(static_cast<uint32_t>(TILE_SUBTRACT));
    module.attr("TILE_MULTIPLY") = py::int_(static_cast<uint32_t>(TILE_MULTIPLY));
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
    PRIMITIVE("TRUE", OP_TRUE); PRIMITIVE("FALSE", OP_FALSE);
    PRIMITIVE("CELLS", OP_CELLS); PRIMITIVE("CELL+", OP_CELL_PLUS);
    PRIMITIVE("EXECUTE", OP_EXECUTE);
    PRIMITIVE("COMPARE", OP_COMPARE); PRIMITIVE("FILL", OP_FILL);
    PRIMITIVE("CMOVE", OP_CMOVE); PRIMITIVE("CMOVE>", OP_CMOVE_UP);
    PRIMITIVE("MOVE", OP_MOVE);
    PRIMITIVE("SP@", OP_SP_FETCH); PRIMITIVE("RP@", OP_RP_FETCH);
    PRIMITIVE("UM*", OP_UMULTIPLY);
    // The original hosted BIOS primitives both push zero. Native admission
    // remains bound to those installed word objects, never a later namesake.
    PRIMITIVE("COREID", OP_FALSE); PRIMITIVE("TASK-ID", OP_FALSE);
    PRIMITIVE("I", OP_I); PRIMITIVE("J", OP_J);
    module.attr("PRIMITIVE_OPCODES") = primitives;
#undef PRIMITIVE
#undef EXPORT_OPCODE
}
