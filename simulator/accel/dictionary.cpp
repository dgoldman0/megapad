// Native semantic dictionary; see dictionary.h and
// docs/simulator-native-evaluator.md.
#include "dictionary.h"

#include <pybind11/stl.h>

#include <algorithm>
#include <cstdio>
#include <stdexcept>

namespace megapad::dictionary {

namespace {
// A Python handle on one published colon body, for lazy IR views.
struct NativeColonBodyHandle {
    std::shared_ptr<const ColonBody> body;
};
}  // namespace

std::string uppercase(const std::string& name) {
    std::string key = name;
    for (char& byte : key) {
        const auto value = static_cast<unsigned char>(byte);
        if (value >= 0x61 && value <= 0x7A) byte = static_cast<char>(value - 0x20);
    }
    return key;
}

uint32_t uppercase_fnv1a32(const char* name, size_t length) {
    uint32_t result = 0x811C9DC5u;
    for (size_t i = 0; i < length; ++i) {
        auto folded = static_cast<unsigned char>(name[i]);
        if (folded >= 0x61 && folded <= 0x7A) folded -= 0x20;
        result ^= folded;
        result *= 0x01000193u;
    }
    return result;
}

namespace {

void require_name(const std::string& name) {
    if (name.empty()) throw py::value_error("dictionary names must not be empty");
    if (name.size() > MAX_NAME_BYTES)
        throw py::value_error("dictionary names may contain at most 127 bytes");
    for (char byte : name) {
        if (static_cast<unsigned char>(byte) >= 0x80)
            throw py::value_error("dictionary names must be ASCII");
    }
}

}  // namespace

NativeDictionary::NativeDictionary(Cell start_address, Cell active_limit, py::object memory,
                                   const py::iterable& regions, Cell page_size,
                                   const py::iterable& dense_regions, bool has_external,
                                   Cell external_base, Cell external_limit)
    : start_address_(start_address),
      active_floor_(start_address),
      active_limit_(active_limit),
      here_(start_address),
      numeric_rollback_floor_(start_address),
      memory_(std::move(memory)),
      has_external_(has_external),
      external_base_(external_base),
      external_limit_(external_limit) {
    if (!memory_.is_none())
        guest_ = guest::GuestMemory(guest::build_regions(regions, page_size, dense_regions),
                                    page_size);
}

void NativeDictionary::protect_current_prefix() {
    if (here_ > numeric_rollback_floor_) numeric_rollback_floor_ = here_;
}

Cell NativeDictionary::checked_advance(Cell width, const char* operation) const {
    if (here_ > CELL_MASK - width)
        throw std::overflow_error(std::string(operation) + " would wrap uint64 addresses");
    const Cell candidate = here_ + width;
    if (candidate > active_limit_)
        throw std::overflow_error(std::string(operation) + " would exceed its memory region");
    return candidate;
}

py::tuple NativeDictionary::define(const std::string& name, const std::string& initial_body,
                                   bool immediate, uint8_t kind, Cell constant,
                                   uint32_t directive, bool has_action, Cell action_xt,
                                   Cell action_ip, py::object implementation) {
    return define_record(name, initial_body, immediate, kind, constant, directive,
                         has_action, action_xt, action_ip, std::move(implementation),
                         nullptr);
}

Cell NativeDictionary::define_native(const std::string& name, uint8_t kind, Cell constant,
                                     const std::string& initial_body,
                                     std::shared_ptr<const ColonBody> body) {
    define_record(name, initial_body, false, kind, constant, 0, false, 0, 0,
                  py::none(), std::move(body));
    return records_.back().xt;
}

py::tuple NativeDictionary::define_record(const std::string& name,
                                          const std::string& initial_body, bool immediate,
                                          uint8_t kind, Cell constant, uint32_t directive,
                                          bool has_action, Cell action_xt, Cell action_ip,
                                          py::object implementation,
                                          std::shared_ptr<const ColonBody> body) {
    require_name(name);
    const Cell allocation_size =
        HEADER_FIXED_BYTES + name.size() + SEMANTIC_CODE_SLOT_BYTES + initial_body.size();
    const Cell allocation_limit = checked_advance(allocation_size, "dictionary definition");

    const Cell header_address = here_;
    const Cell xt = header_address + HEADER_FIXED_BYTES + name.size();
    if (xt == 0)
        throw std::overflow_error("dictionary definition produced execution token zero");

    // Normal forward publication is beyond every immutable live header.
    // Rewinds and switches to lower arenas retain the full overlap proof,
    // including a new definition's initial body crossing a later header.
    if (header_address < header_limit_) {
        for (const Record& live : records_) {
            if (header_address < live.body_address() && live.header_address < allocation_limit)
                throw py::value_error(
                    "dictionary definition would overlap a live header or code slot");
        }
    }

    const Cell link = latest();
    std::string header;
    header.reserve(static_cast<size_t>(allocation_size));
    for (int i = 0; i < 8; ++i) header.push_back(static_cast<char>(link >> (8 * i)));
    header.push_back(static_cast<char>(name.size() | (immediate ? IMMEDIATE_FLAG : 0)));
    header += name;
    header.append(SEMANTIC_CODE_SLOT_BYTES, '\0');
    header += initial_body;
    prepare_body_write(header_address, allocation_limit);
    if (attached()) {
        // Publish metadata only after the complete guest-visible header has
        // passed memory preflight and been emitted contiguously.
        write_bytes(header_address, reinterpret_cast<const uint8_t*>(header.data()),
                    header.size());
    }

    const auto index = static_cast<uint32_t>(records_.size());
    Record record;
    record.name = name;
    record.key = uppercase(name);
    record.header_address = header_address;
    record.xt = xt;
    record.immediate = immediate;
    record.kind = kind;
    record.constant = constant;
    record.directive = directive;
    record.has_action = has_action;
    record.action_xt = action_xt;
    record.action_ip = action_ip;
    record.uid = ++next_uid_;
    record.implementation = std::move(implementation);
    record.body = std::move(body);
    bindings_[record.key].push_back(index);
    by_xt_[xt] = index;
    records_.push_back(std::move(record));
    if (!initial_body.empty()) {
        ++allocation_serial_;
        allocations_[xt] = Allocation{xt + SEMANTIC_CODE_SLOT_BYTES, allocation_limit,
                                      allocation_serial_};
        allocation_limit_ = std::max(allocation_limit_, allocation_limit);
    }
    header_limit_ = std::max(header_limit_, xt + SEMANTIC_CODE_SLOT_BYTES);
    here_ = allocation_limit;
    ++generation_;
    return py::make_tuple(index, header_address, xt, records_.back().uid);
}

void NativeDictionary::allot(int64_t delta) {
    const __int128 candidate = static_cast<__int128>(here_) + delta;
    if (candidate < static_cast<__int128>(active_floor_))
        throw std::overflow_error("ALLOT would move HERE below its active dictionary zone");
    if (candidate > static_cast<__int128>(CELL_MASK))
        throw std::overflow_error("ALLOT would wrap the uint64 address space");
    if (candidate > static_cast<__int128>(active_limit_))
        throw std::overflow_error("ALLOT would move HERE beyond its memory region");
    const auto target = static_cast<Cell>(candidate);
    revoke_span(target, here_);
    here_ = target;
}

void NativeDictionary::move_here(Cell target, Cell floor, Cell limit) {
    // Only the selected zone's writable tail becomes available for reuse.
    revoke_span(target, limit);
    active_floor_ = floor;
    active_limit_ = limit;
    here_ = target;
}

void NativeDictionary::store_and_advance(Cell value, unsigned width, bool is_integer) {
    const Cell candidate = checked_advance(width, "dictionary store");
    if (!is_integer) throw py::type_error("stored value must be an integer");
    prepare_body_write(here_, candidate);
    if (width == 8) {
        write64(here_, value);
    } else {
        const auto byte = static_cast<uint8_t>(value);
        write_bytes(here_, &byte, 1);
    }
    here_ = candidate;
}

Cell NativeDictionary::write_transient(const std::string& payload) {
    const Cell limit = checked_advance(payload.size(), "transient dictionary write");
    prepare_body_write(here_, limit);
    write_bytes(here_, reinterpret_cast<const uint8_t*>(payload.data()), payload.size());
    return here_;
}

py::object NativeDictionary::view(size_t index) const {
    const Record& record = records_.at(index);
    if (record.view) return record.view;
    return py::int_(index);
}

py::object NativeDictionary::find(const std::string& name) const {
    require_name(name);
    const auto found = bindings_.find(uppercase(name));
    if (found == bindings_.end() || found->second.empty()) return py::none();
    return view(found->second.back());
}

long NativeDictionary::lookup(const char* name, size_t length) const {
    if (length == 0 || length > MAX_NAME_BYTES) return -1;
    std::string key(name, length);
    for (char& byte : key) {
        const auto value = static_cast<unsigned char>(byte);
        if (value >= 0x80) return -1;
        if (value >= 0x61 && value <= 0x7A) byte = static_cast<char>(value - 0x20);
    }
    const auto found = bindings_.find(key);
    if (found == bindings_.end() || found->second.empty()) return -1;
    return static_cast<long>(found->second.back());
}

long NativeDictionary::index_of_xt(Cell xt) const {
    const auto found = by_xt_.find(xt);
    return found == by_xt_.end() ? -1 : static_cast<long>(found->second);
}

py::object NativeDictionary::resolve(Cell xt) const {
    const auto found = by_xt_.find(xt);
    if (found == by_xt_.end()) {
        char message[64];
        std::snprintf(message, sizeof(message), "unknown execution token 0x%016llx",
                      static_cast<unsigned long long>(xt));
        throw py::key_error(message);
    }
    return view(found->second);
}

py::object NativeDictionary::live(Cell xt) const {
    const auto found = by_xt_.find(xt);
    if (found == by_xt_.end()) return py::none();
    return view(found->second);
}

py::list NativeDictionary::views() const {
    py::list result;
    for (size_t i = 0; i < records_.size(); ++i) result.append(view(i));
    return result;
}

py::tuple NativeDictionary::record_info(size_t index) const {
    const Record& r = records_.at(index);
    py::object implementation = r.implementation ? r.implementation : py::object(py::none());
    py::object body = r.body ? py::cast(NativeColonBodyHandle{r.body}) : py::object(py::none());
    return py::make_tuple(py::bytes(r.name), r.header_address, r.xt, r.immediate,
                          static_cast<int>(r.kind), r.constant, r.directive, r.has_action,
                          r.action_xt, r.action_ip, r.uid, implementation, body);
}

void NativeDictionary::set_view(size_t index, py::object view) {
    records_.at(index).view = std::move(view);
}

void NativeDictionary::set_action(Cell xt, uint64_t uid, bool has_action, Cell action_xt,
                                  Cell action_ip) {
    const auto found = by_xt_.find(xt);
    if (found == by_xt_.end()) return;
    Record& record = records_[found->second];
    if (record.uid != uid) return;
    record.has_action = has_action;
    record.action_xt = action_xt;
    record.action_ip = action_ip;
}

py::object NativeDictionary::allocation(Cell xt) const {
    const auto found = allocations_.find(xt);
    if (found == allocations_.end()) return py::none();
    return py::make_tuple(found->second.body_address, found->second.body_limit,
                          found->second.serial);
}

std::vector<Cell> NativeDictionary::overlapping_allocations(Cell address, Cell limit) const {
    // Forward compilation normally starts beyond all existing bodies.
    // Keep that common path constant time rather than scanning all words.
    std::vector<Cell> result;
    if (address >= limit || address >= allocation_limit_) return result;
    for (const auto& [xt, allocation] : allocations_) {
        if (address < allocation.body_limit && allocation.body_address < limit)
            result.push_back(xt);
    }
    return result;
}

void NativeDictionary::revoke_span(Cell address, Cell limit) {
    revoke_allocations(overlapping_allocations(address, limit));
}

void NativeDictionary::revoke_allocations(const std::vector<Cell>& xts) {
    if (xts.empty()) return;
    for (Cell xt : xts) allocations_.erase(xt);
    allocation_limit_ = 0;
    for (const auto& [xt, allocation] : allocations_)
        allocation_limit_ = std::max(allocation_limit_, allocation.body_limit);
    // A reclaimed body may hold machine code that native plans call.
    ++generation_;
}

void NativeDictionary::prepare_body_write(Cell address, Cell limit) {
    const auto xts = overlapping_allocations(address, limit);
    if (xts.empty()) return;
    if (attached() && !guest_.contains(address, limit - address))
        memory_.attr("_qualify_ordinary_span")(address, limit - address);
    revoke_allocations(xts);
}

std::vector<Cell> NativeDictionary::publish_rollback(size_t depth, Cell here, bool zone,
                                                     Cell floor, Cell limit) {
    // Prove every binding suffix before removing the first entry, so a
    // detected metadata inconsistency stays atomic.
    std::unordered_map<std::string, std::vector<uint32_t>> removed_bindings;
    for (size_t i = depth; i < records_.size(); ++i) {
        const Record& record = records_[i];
        removed_bindings[record.key].push_back(static_cast<uint32_t>(i));
        const auto found = by_xt_.find(record.xt);
        if (found == by_xt_.end() || found->second != i)
            throw std::runtime_error("dictionary execution-token history is inconsistent");
    }
    for (const auto& [key, removed] : removed_bindings) {
        const auto found = bindings_.find(key);
        if (found == bindings_.end() || found->second.size() < removed.size())
            throw std::runtime_error("dictionary binding history is inconsistent");
        const auto& stack = found->second;
        const size_t offset = stack.size() - removed.size();
        for (size_t k = 0; k < removed.size(); ++k) {
            if (stack[offset + k] != removed[k])
                throw std::runtime_error("dictionary binding history is inconsistent");
        }
    }

    // Finish every lineage check before revoking allocations. Removals
    // revoke even when LATEST! leaves HERE unchanged, while a retained word
    // can lose only its body through a partial frontier rewind.
    revoke_span(here, zone ? limit : here_);
    std::vector<Cell> removed_allocations;
    for (size_t i = depth; i < records_.size(); ++i) {
        if (allocations_.count(records_[i].xt)) removed_allocations.push_back(records_[i].xt);
    }
    revoke_allocations(removed_allocations);

    for (size_t i = records_.size(); i > depth; --i) {
        const Record& record = records_[i - 1];
        auto found = bindings_.find(record.key);
        found->second.pop_back();
        if (found->second.empty()) bindings_.erase(found);
        by_xt_.erase(record.xt);
    }
    std::vector<Cell> removed;
    for (size_t i = depth; i < records_.size(); ++i) removed.push_back(records_[i].xt);
    records_.erase(records_.begin() + static_cast<std::ptrdiff_t>(depth), records_.end());
    if (!removed.empty()) {
        header_limit_ = 0;
        for (const Record& record : records_)
            header_limit_ = std::max(header_limit_, record.body_address());
    }
    if (zone) {
        active_floor_ = floor;
        active_limit_ = limit;
    }
    here_ = here;
    ++generation_;
    return removed;
}

std::vector<Cell> NativeDictionary::rollback_to(Cell saved_here, Cell saved_latest) {
    if (saved_here < active_floor_ || saved_here > active_limit_)
        throw py::value_error("saved HERE is outside the dictionary region");
    if (saved_here < numeric_rollback_floor_)
        throw py::value_error("saved HERE is below the protected dictionary prefix");
    if (saved_here > here_)
        throw py::value_error("saved HERE is ahead of the active dictionary");

    // Native rollback proves ancestry by walking the guest-visible links,
    // not by trusting an accelerator or side table. Hosted metadata has no
    // representation for a manually spliced chain, so require the live links
    // to describe the complete semantic definition order exactly.
    if (attached()) {
        Cell cursor = latest();
        for (size_t i = records_.size(); i > 0; --i) {
            if (cursor != records_[i - 1].header_address)
                throw py::value_error("live dictionary link history is inconsistent");
            cursor = read64(cursor);
        }
        if (cursor != 0) throw py::value_error("live dictionary link history is inconsistent");
    }

    size_t target_depth = 0;
    if (saved_latest != 0) {
        bool found = false;
        for (size_t i = 0; i < records_.size(); ++i) {
            if (records_[i].header_address == saved_latest) {
                target_depth = i + 1;
                found = true;
                break;
            }
        }
        if (!found) throw py::value_error("saved LATEST is not a live dictionary ancestor");
    }
    for (size_t i = target_depth; i < records_.size(); ++i) {
        const Cell header = records_[i].header_address;
        if (!(saved_here <= header && header < here_))
            throw py::value_error("removed dictionary header is outside the reclaimed interval");
    }
    for (size_t i = 0; i < target_depth; ++i) {
        const Cell header = records_[i].header_address;
        if (saved_here <= header && header < here_)
            throw py::value_error("retained dictionary header is inside the reclaimed interval");
    }
    return publish_rollback(target_depth, saved_here, false, 0, 0);
}

std::vector<Cell> NativeDictionary::set_latest(Cell target_latest) {
    size_t target_depth = 0;
    if (target_latest != 0) {
        bool found = false;
        for (size_t i = 0; i < records_.size(); ++i) {
            if (records_[i].header_address == target_latest) {
                target_depth = i + 1;
                found = true;
                break;
            }
        }
        if (!found) throw py::value_error("LATEST! target has no hosted semantic definition");
    }
    Cell cursor = target_latest;
    for (size_t i = target_depth; i > 0; --i) {
        const size_t index = i - 1;
        if (cursor != records_[index].header_address)
            throw py::value_error("retained dictionary link history is inconsistent");
        if (attached()) cursor = read64(cursor);
        else cursor = index == 0 ? 0 : records_[index - 1].header_address;
    }
    if (cursor != 0) throw py::value_error("retained dictionary link history is inconsistent");
    return publish_rollback(target_depth, here_, false, 0, 0);
}

void NativeDictionary::read_bytes(Cell address, uint8_t* out, Cell length) {
    if (length == 0) return;
    if (guest_.contains(address, length)) {
        guest_.read(address, out, length);
        return;
    }
    const std::string data = py::cast<std::string>(memory_.attr("read_bytes")(address, length));
    std::copy(data.begin(), data.end(), out);
}

void NativeDictionary::write_bytes(Cell address, const uint8_t* data, Cell length) {
    if (length == 0) return;
    if (guest_.contains(address, length)) {
        guest_.write(address, data, length);
        return;
    }
    memory_.attr("write_bytes")(address,
        py::bytes(reinterpret_cast<const char*>(data), static_cast<size_t>(length)));
}

Cell NativeDictionary::read64(Cell address) {
    if (guest_.contains(address, 8)) return guest_.read64(address);
    return memory_.attr("read64")(address).cast<Cell>();
}

uint8_t NativeDictionary::read8(Cell address) {
    if (guest_.contains(address, 1)) return guest_.read8(address);
    return memory_.attr("read8")(address).cast<uint8_t>();
}

py::list ir_tuples(const ColonBody& body) {
    py::list result;
    for (const IROp& op : body.ops) {
        py::object payload = py::none();
        if (op.kind == IR_WRITE_OUTPUT || op.kind == IR_ABORT_IF)
            payload = py::bytes(body.payloads.at(static_cast<size_t>(op.a)));
        result.append(py::make_tuple(static_cast<int>(op.kind), op.a, op.b, payload));
    }
    return result;
}

void NativeDictionary::write64(Cell address, Cell value) {
    if (guest_.contains(address, 8)) {
        guest_.write64(address, value);
        return;
    }
    memory_.attr("write64")(address, value);
}

py::tuple NativeDictionary::index_state() const {
    return py::make_tuple(index_.base, index_.slots, index_.count, index_.flags);
}

py::tuple NativeDictionary::index_notification() const {
    return py::make_tuple(notify_count_, notify_xt_);
}

void NativeDictionary::index_arm(Cell count, Cell xt) {
    notify_count_ = count;
    notify_xt_ = xt;
}

Cell NativeDictionary::index_take() {
    const Cell xt = notify_xt_;
    if (xt == 0 || index_.count < notify_count_) return 0;
    notify_count_ = 0;
    notify_xt_ = 0;
    return xt;
}

int NativeDictionary::index_configure(Cell base, Cell slots) {
    if (slots == 0) {
        if (base != 0) return 1;
        // BIOS disable leaves the old caller bytes untouched.
        index_ = IndexState{};
        return 0;
    }
    if (base == 0 || (base & (INDEX_SLOT_BYTES - 1)) != 0) return 1;
    if ((slots & (slots - 1)) != 0) return 1;
    if (slots > CELL_MASK / INDEX_SLOT_BYTES) return 1;
    const Cell span = slots * INDEX_SLOT_BYTES;
    if (base > CELL_MASK - span) return 1;
    const Cell limit = base + span;
    if (!has_external_ || base < external_base_ || limit > external_limit_) return 1;
    // Installation publishes the new geometry before rebuilding.
    index_ = IndexState{base, slots, 0, INDEX_BOUND | INDEX_BUILDING};
    return index_rebuild();
}

int NativeDictionary::index_rebuild() {
    if ((index_.flags & INDEX_BOUND) == 0) return 0;
    index_ = IndexState{index_.base, index_.slots, 0, INDEX_BOUND | INDEX_BUILDING};
    const Cell span = index_.slots * INDEX_SLOT_BYTES;
    if (guest_.contains(index_.base, span)) guest_.fill(index_.base, span, 0);
    else memory_.attr("fill")(index_.base, span, 0);
    for (size_t i = records_.size(); i > 0; --i) {
        const Record& record = records_[i - 1];
        if (!index_insert(record.name, record.header_address, false)) {
            index_.flags = INDEX_BOUND | INDEX_SATURATED;
            return 2;
        }
    }
    index_.flags = INDEX_BOUND | INDEX_AUTHORITATIVE;
    return 0;
}

void NativeDictionary::index_publish(const std::string& name, Cell header_address) {
    if ((index_.flags & INDEX_BOUND) == 0) return;
    const bool was_saturated = (index_.flags & INDEX_SATURATED) != 0;
    index_.flags = INDEX_BOUND | INDEX_BUILDING | (was_saturated ? INDEX_SATURATED : 0);
    const bool inserted = index_insert(name, header_address, true);
    index_.flags = was_saturated || !inserted ? INDEX_BOUND | INDEX_SATURATED
                                              : INDEX_BOUND | INDEX_AUTHORITATIVE;
}

bool NativeDictionary::index_insert(const std::string& name, Cell header_address, bool upsert) {
    const uint32_t hash = uppercase_fnv1a32(name.data(), name.size());
    Cell slot = 0;
    bool exact = false;
    if (!index_probe(name, hash, slot, exact)) return false;
    if (exact) {
        if (upsert) write64(slot, header_address);
        return true;
    }
    // Match BIOS publication order: metadata first, entry pointer last.
    write64(slot + 8, Cell{hash} | (static_cast<Cell>(name.size()) << 32));
    write64(slot, header_address);
    ++index_.count;
    return true;
}

bool NativeDictionary::index_probe(const std::string& name, uint32_t hash, Cell& slot,
                                   bool& exact) {
    const Cell mask = index_.slots - 1;
    Cell slot_index = hash & mask;
    for (Cell probe = 0; probe < index_.slots; ++probe) {
        const Cell at = index_.base + slot_index * INDEX_SLOT_BYTES;
        const Cell entry = read64(at);
        if (entry == 0) {
            slot = at;
            exact = false;
            return true;
        }
        const Cell metadata = read64(at + 8);
        const Cell stored_hash = metadata & 0xFFFFFFFFu;
        const Cell stored_length = (metadata >> 32) & 0x7F;
        if (stored_hash == hash && stored_length == name.size()) {
            uint8_t flags_length = 0;
            read_bytes(entry + 8, &flags_length, 1);
            if ((flags_length & 0x7F) == name.size()) {
                std::string candidate(name.size(), '\0');
                read_bytes(entry + 9, reinterpret_cast<uint8_t*>(candidate.data()),
                           candidate.size());
                if (uppercase(candidate) == uppercase(name)) {
                    slot = at;
                    exact = true;
                    return true;
                }
            }
        }
        slot_index = (slot_index + 1) & mask;
    }
    return false;
}

void register_bindings(py::module_& module) {
    py::class_<NativeColonBodyHandle>(module, "NativeColonBody")
        .def("operations", [](const NativeColonBodyHandle& self) {
            return ir_tuples(*self.body);
        });
    py::class_<NativeDictionary>(module, "NativeDictionary")
        .def(py::init<Cell, Cell, py::object, const py::iterable&, Cell, const py::iterable&,
                      bool, Cell, Cell>(),
             py::arg("start_address"), py::arg("active_limit"), py::arg("memory"),
             py::arg("regions"), py::arg("page_size"), py::arg("dense_regions"),
             py::arg("has_external"), py::arg("external_base"), py::arg("external_limit"))
        .def_property_readonly("here", &NativeDictionary::here)
        .def_property_readonly("start_address", &NativeDictionary::start_address)
        .def_property_readonly("active_floor", &NativeDictionary::active_floor)
        .def_property_readonly("active_limit", &NativeDictionary::active_limit)
        .def_property_readonly("numeric_rollback_floor",
                               &NativeDictionary::numeric_rollback_floor)
        .def_property_readonly("execution_generation", &NativeDictionary::execution_generation)
        .def_property_readonly("latest", &NativeDictionary::latest)
        .def("__len__", &NativeDictionary::size)
        .def("protect_current_prefix", &NativeDictionary::protect_current_prefix)
        .def("define", [](NativeDictionary& self, const py::bytes& name,
                          const py::bytes& initial_body, bool immediate, int kind,
                          Cell constant, uint32_t directive, bool has_action, Cell action_xt,
                          Cell action_ip, py::object implementation) {
                 return self.define(name, initial_body, immediate,
                                    static_cast<uint8_t>(kind), constant, directive,
                                    has_action, action_xt, action_ip,
                                    std::move(implementation));
             })
        .def("allot", &NativeDictionary::allot)
        .def("move_here", &NativeDictionary::move_here)
        .def("store_and_advance", &NativeDictionary::store_and_advance)
        .def("write_transient", [](NativeDictionary& self, const py::bytes& payload) {
                 return self.write_transient(payload);
             })
        .def("find", [](const NativeDictionary& self, const py::bytes& name) {
                 return self.find(name);
             })
        .def("resolve", &NativeDictionary::resolve)
        .def("live", &NativeDictionary::live)
        .def("view", &NativeDictionary::view)
        .def("views", &NativeDictionary::views)
        .def("record_info", &NativeDictionary::record_info)
        .def("set_view", &NativeDictionary::set_view)
        .def("set_action", &NativeDictionary::set_action)
        .def("allocation", &NativeDictionary::allocation)
        .def("publish_rollback", &NativeDictionary::publish_rollback)
        .def("rollback_to", &NativeDictionary::rollback_to)
        .def("set_latest", &NativeDictionary::set_latest)
        .def("index_state", &NativeDictionary::index_state)
        .def("index_notification", &NativeDictionary::index_notification)
        .def("index_arm", &NativeDictionary::index_arm)
        .def("index_take", &NativeDictionary::index_take)
        .def("index_configure", &NativeDictionary::index_configure)
        .def("index_rebuild", &NativeDictionary::index_rebuild)
        .def("index_publish", [](NativeDictionary& self, const py::bytes& name,
                                 Cell header_address) {
                 self.index_publish(name, header_address);
             });
    module.attr("KIND_OPAQUE") = py::int_(static_cast<int>(KIND_OPAQUE));
    module.attr("KIND_PRIMITIVE") = py::int_(static_cast<int>(KIND_PRIMITIVE));
    module.attr("KIND_COLON") = py::int_(static_cast<int>(KIND_COLON));
    module.attr("KIND_CONSTANT") = py::int_(static_cast<int>(KIND_CONSTANT));
    module.attr("KIND_VALUE") = py::int_(static_cast<int>(KIND_VALUE));
    module.attr("KIND_CREATED") = py::int_(static_cast<int>(KIND_CREATED));
    module.attr("KIND_DIRECTIVE") = py::int_(static_cast<int>(KIND_DIRECTIVE));
    module.attr("KIND_ROUTINE") = py::int_(static_cast<int>(KIND_ROUTINE));
#define EXPORT_IR(name) module.attr(#name) = py::int_(static_cast<int>(name))
    EXPORT_IR(IR_LITERAL); EXPORT_IR(IR_CALL); EXPORT_IR(IR_CALL_SELF);
    EXPORT_IR(IR_STORE_VALUE); EXPORT_IR(IR_BRANCH); EXPORT_IR(IR_BRANCH_ZERO);
    EXPORT_IR(IR_RETURN); EXPORT_IR(IR_IDLE); EXPORT_IR(IR_IDLE_UNTIL);
    EXPORT_IR(IR_UART_READ); EXPORT_IR(IR_R_PUSH); EXPORT_IR(IR_R_POP);
    EXPORT_IR(IR_R_PEEK); EXPORT_IR(IR_R_PUSH_PAIR); EXPORT_IR(IR_R_POP_PAIR);
    EXPORT_IR(IR_R_PEEK_PAIR); EXPORT_IR(IR_STRING_LITERAL); EXPORT_IR(IR_DO);
    EXPORT_IR(IR_QUESTION_DO); EXPORT_IR(IR_LOOP); EXPORT_IR(IR_PLUS_LOOP);
    EXPORT_IR(IR_UNLOOP); EXPORT_IR(IR_INSTALL_DOES); EXPORT_IR(IR_RESTORE_DSP);
    EXPORT_IR(IR_RESTORE_RSP); EXPORT_IR(IR_WRITE_OUTPUT); EXPORT_IR(IR_ABORT_IF);
#undef EXPORT_IR
}

}  // namespace megapad::dictionary
