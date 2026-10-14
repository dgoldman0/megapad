// Native semantic dictionary for the hosted simulator's native backend.
//
// This is the authoritative host-side dictionary metadata when the native
// backend is selected: definition order, case-insensitive bindings, the XT
// map, HERE and its zone, body allocations, the execution generation and the
// caller-backed side index. It mirrors simulator/dictionary.py and
// simulator/dictionary_index.py exactly, including their checks, mutation
// order and exception messages; simulator/native_dictionary.py presents it
// through the same Python interface. Guest bytes are written through the
// shared region views, never a copy.
#pragma once

#include "guest_memory.h"

#include <pybind11/pybind11.h>

#include <cstdint>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace megapad::dictionary {
namespace py = pybind11;
using guest::Cell;
using guest::CELL_MASK;

constexpr Cell HEADER_FIXED_BYTES = 9;
constexpr Cell SEMANTIC_CODE_SLOT_BYTES = 8;
constexpr uint8_t IMMEDIATE_FLAG = 0x80;
constexpr size_t MAX_NAME_BYTES = 0x7F;

// Kinds the native code can interpret. Any other implementation object is
// opaque: kept for Python and never interpreted natively.
enum Kind : uint8_t {
    KIND_OPAQUE,
    KIND_PRIMITIVE,
    KIND_COLON,
    KIND_CONSTANT,
    KIND_VALUE,
    KIND_CREATED,
    KIND_DIRECTIVE,
    KIND_ROUTINE,
};

// Semantic IR as simulator/ir.py defines it. Operands are the IR fields;
// WRITE_OUTPUT and ABORT_IF keep their payload index in `a`.
enum IRKind : uint8_t {
    IR_LITERAL,
    IR_CALL,
    IR_CALL_SELF,
    IR_STORE_VALUE,
    IR_BRANCH,
    IR_BRANCH_ZERO,
    IR_RETURN,
    IR_IDLE,
    IR_IDLE_UNTIL,
    IR_UART_READ,
    IR_R_PUSH,
    IR_R_POP,
    IR_R_PEEK,
    IR_R_PUSH_PAIR,
    IR_R_POP_PAIR,
    IR_R_PEEK_PAIR,
    IR_STRING_LITERAL,
    IR_DO,
    IR_QUESTION_DO,
    IR_LOOP,
    IR_PLUS_LOOP,
    IR_UNLOOP,
    IR_INSTALL_DOES,
    IR_RESTORE_DSP,
    IR_RESTORE_RSP,
    IR_WRITE_OUTPUT,
    IR_ABORT_IF,
    IR_KIND_COUNT,
};

struct IROp {
    uint8_t kind;
    Cell a;
    Cell b;
};

// The compiled body of a colon definition the native evaluator published.
// Shared so a Python view can still read it after its record is removed.
struct ColonBody {
    std::vector<IROp> ops;
    std::vector<std::string> payloads;
};

py::list ir_tuples(const ColonBody& body);

struct Record {
    std::string name;
    std::string key;
    Cell header_address = 0;
    Cell xt = 0;
    bool immediate = false;
    uint8_t kind = KIND_OPAQUE;
    Cell constant = 0;
    uint32_t directive = 0;
    bool has_action = false;
    Cell action_xt = 0;
    Cell action_ip = 0;
    // Distinguishes this definition from any later one at the same XT.
    uint64_t uid = 0;
    // The Python implementation object, when Python supplied one.
    py::object implementation;
    // The cached Python Word view, created on first Python access.
    py::object view;
    // The native IR of a colon definition the native evaluator compiled.
    std::shared_ptr<const ColonBody> body;

    Cell body_address() const { return xt + SEMANTIC_CODE_SLOT_BYTES; }
};

struct Allocation {
    Cell body_address;
    Cell body_limit;
    uint64_t serial;
};

constexpr Cell INDEX_BOUND = 1 << 0;
constexpr Cell INDEX_AUTHORITATIVE = 1 << 1;
constexpr Cell INDEX_BUILDING = 1 << 2;
constexpr Cell INDEX_SATURATED = 1 << 3;
constexpr Cell INDEX_SLOT_BYTES = 16;

struct IndexState {
    Cell base = 0;
    Cell slots = 0;
    Cell count = 0;
    Cell flags = 0;
};

class NativeDictionary {
public:
    // `memory` is the Python SparseAddressSpace (or None). Its regions are
    // passed separately as the same descriptors NativeProgram receives.
    NativeDictionary(Cell start_address, Cell active_limit, py::object memory,
                     const py::iterable& regions, Cell page_size,
                     const py::iterable& dense_regions, bool has_external,
                     Cell external_base, Cell external_limit);

    // State.
    Cell here() const { return here_; }
    Cell start_address() const { return start_address_; }
    Cell active_floor() const { return active_floor_; }
    Cell active_limit() const { return active_limit_; }
    Cell numeric_rollback_floor() const { return numeric_rollback_floor_; }
    Cell execution_generation() const { return generation_; }
    size_t size() const { return records_.size(); }
    Cell latest() const { return records_.empty() ? 0 : records_.back().header_address; }
    bool attached() const { return !memory_.is_none(); }
    const Record& record(size_t index) const { return records_[index]; }

    void protect_current_prefix();

    // Publication and growth.
    py::tuple define(const std::string& name, const std::string& initial_body,
                     bool immediate, uint8_t kind, Cell constant, uint32_t directive,
                     bool has_action, Cell action_xt, Cell action_ip,
                     py::object implementation);
    // Publish a definition the native evaluator compiled or created. The
    // caller has already proven growth; the Python view is built on demand.
    Cell define_native(const std::string& name, uint8_t kind, Cell constant,
                       const std::string& initial_body,
                       std::shared_ptr<const ColonBody> body);
    void allot(int64_t delta);
    void move_here(Cell target, Cell floor, Cell limit);
    void store_and_advance(Cell value, unsigned width, bool is_integer);
    Cell write_transient(const std::string& payload);

    // Lookup. A result is the cached view, or the record index when Python
    // has not asked for this record yet.
    py::object find(const std::string& name) const;
    py::object resolve(Cell xt) const;
    py::object live(Cell xt) const;
    py::object view(size_t index) const;
    py::list views() const;
    py::tuple record_info(size_t index) const;
    void set_view(size_t index, py::object view);

    // Created-word actions, written only for the exact live definition.
    void set_action(Cell xt, uint64_t uid, bool has_action, Cell action_xt, Cell action_ip);

    // Body allocations.
    py::object allocation(Cell xt) const;

    // Rollback. The Python facade checks opaque checkpoint seals first.
    // Each returns the XTs of the removed definitions, oldest first, so the
    // facade can drop their cached views.
    std::vector<Cell> publish_rollback(size_t depth, Cell here, bool zone, Cell floor,
                                       Cell limit);
    std::vector<Cell> rollback_to(Cell saved_here, Cell saved_latest);
    std::vector<Cell> set_latest(Cell latest);

    // Side index.
    py::tuple index_state() const;
    py::tuple index_notification() const;
    void index_arm(Cell count, Cell xt);
    Cell index_take();
    int index_configure(Cell base, Cell slots);
    int index_rebuild();
    void index_publish(const std::string& name, Cell header_address);

    // Lookup for native callers: the newest live record, or -1.
    long lookup(const char* name, size_t length) const;
    long index_of_xt(Cell xt) const;
    Cell header_limit() const { return header_limit_; }
    Cell index_count() const { return index_.count; }
    Cell notify_count() const { return notify_count_; }
    Cell notify_xt() const { return notify_xt_; }

    // Guest access shared with the native evaluator.
    guest::GuestMemory& guest() { return guest_; }
    void read_bytes(Cell address, uint8_t* out, Cell length);
    void write_bytes(Cell address, const uint8_t* data, Cell length);
    Cell read64(Cell address);
    void write64(Cell address, Cell value);
    uint8_t read8(Cell address);

private:
    Cell checked_advance(Cell width, const char* operation) const;
    std::vector<Cell> overlapping_allocations(Cell address, Cell limit) const;
    void revoke_span(Cell address, Cell limit);
    void revoke_allocations(const std::vector<Cell>& xts);
    void prepare_body_write(Cell address, Cell limit);

    py::tuple define_record(const std::string& name, const std::string& initial_body,
                            bool immediate, uint8_t kind, Cell constant, uint32_t directive,
                            bool has_action, Cell action_xt, Cell action_ip,
                            py::object implementation,
                            std::shared_ptr<const ColonBody> body);

    bool index_insert(const std::string& name, Cell header_address, bool upsert);
    bool index_probe(const std::string& name, uint32_t hash, Cell& slot, bool& exact);

    Cell start_address_;
    Cell active_floor_;
    Cell active_limit_;
    Cell here_;
    Cell numeric_rollback_floor_;
    Cell header_limit_ = 0;
    Cell generation_ = 0;
    uint64_t next_uid_ = 0;
    std::vector<Record> records_;
    std::unordered_map<std::string, std::vector<uint32_t>> bindings_;
    std::unordered_map<Cell, uint32_t> by_xt_;
    std::unordered_map<Cell, Allocation> allocations_;
    Cell allocation_limit_ = 0;
    uint64_t allocation_serial_ = 0;

    py::object memory_;
    guest::GuestMemory guest_;
    bool has_external_;
    Cell external_base_;
    Cell external_limit_;
    IndexState index_;
    Cell notify_count_ = 0;
    Cell notify_xt_ = 0;
};

std::string uppercase(const std::string& name);
uint32_t uppercase_fnv1a32(const char* name, size_t length);

void register_bindings(py::module_& module);

}  // namespace megapad::dictionary
