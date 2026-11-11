#pragma once

// Python boundary of the chiplet-link model, shared by both extensions. The
// model itself has no Python dependency. Each extension registers these
// classes for itself (module_local), so a process may load both.
//
// MailboxPending is the participant table. ChipletLink is the whole of
// MegaPad's side of the link over a caller's buffer, with no machine around
// it: the table, the register window and its ports. ManualChiplet is a
// chiplet driven from Python, for tests that issue requests as the chiplet.
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include <cstring>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#include "chiplet_module.h"
#include "chiplet_port.h"
#include "mailbox_pending.h"

namespace megapad::chiplet {

// External memory over a Python buffer, with one injectable error.
class BufferMemory final : public ExternalMemory {
public:
    BufferMemory(pybind11::buffer buffer, std::uint64_t base)
        : info_(buffer.request(true)), base_(base) {
        if (info_.itemsize != 1 || info_.ndim != 1)
            throw pybind11::value_error(
                "external memory must be a flat buffer of bytes");
        if ((base & 7) != 0 || (info_.size & 7) != 0)
            throw pybind11::value_error(
                "external memory must start and end on a 64-bit word");
        if (base + static_cast<std::uint64_t>(info_.size) < base)
            throw pybind11::value_error(
                "external memory must fit in the address space");
    }

    std::uint64_t base() const noexcept override { return base_; }
    std::uint64_t limit() const noexcept override {
        return base_ + static_cast<std::uint64_t>(info_.size);
    }
    bool read(std::uint64_t addr, std::uint8_t* out,
              std::size_t bytes) noexcept override {
        if (fails(addr, bytes))
            return false;
        std::memcpy(out, data() + (addr - base_), bytes);
        return true;
    }
    bool write(std::uint64_t addr, const std::uint8_t* in,
               std::size_t bytes) noexcept override {
        if (fails(addr, bytes))
            return false;
        std::memcpy(data() + (addr - base_), in, bytes);
        return true;
    }

    // Accesses that touch this byte report a memory error.
    void fail_at(std::optional<std::uint64_t> addr) noexcept {
        fail_at_ = addr;
    }

private:
    std::uint8_t* data() const noexcept {
        return static_cast<std::uint8_t*>(info_.ptr);
    }
    bool fails(std::uint64_t addr, std::size_t bytes) const noexcept {
        return fail_at_.has_value() && *fail_at_ >= addr &&
            *fail_at_ - addr < bytes;
    }

    pybind11::buffer_info info_;
    std::uint64_t base_;
    std::optional<std::uint64_t> fail_at_;
};

// External memory reached through two Python callables, for a backend whose
// memory has no single buffer: reader(addr, length) returns that many bytes
// and writer(addr, payload) stores them. A callable that raises, or returns
// another length, reports a memory error to the chiplet; the exception is
// handed to Python's unraisable hook, so it is seen.
class CallbackMemory final : public ExternalMemory {
public:
    CallbackMemory(pybind11::object reader, pybind11::object writer,
                   std::uint64_t base, std::uint64_t size)
        : reader_(std::move(reader)), writer_(std::move(writer)),
          base_(base), size_(size) {
        if ((base & 7) != 0 || (size & 7) != 0)
            throw pybind11::value_error(
                "external memory must start and end on a 64-bit word");
        if (base + size < base)
            throw pybind11::value_error(
                "external memory must fit in the address space");
    }

    std::uint64_t base() const noexcept override { return base_; }
    std::uint64_t limit() const noexcept override { return base_ + size_; }

    bool read(std::uint64_t addr, std::uint8_t* out,
              std::size_t bytes) noexcept override {
        pybind11::gil_scoped_acquire gil;
        try {
            const pybind11::bytes data = reader_(addr, bytes);
            char* text = nullptr;
            Py_ssize_t length = 0;
            if (PyBytes_AsStringAndSize(data.ptr(), &text, &length) != 0)
                throw pybind11::error_already_set();
            if (static_cast<std::size_t>(length) != bytes)
                return false;
            std::memcpy(out, text, bytes);
            return true;
        } catch (pybind11::error_already_set& error) {
            error.discard_as_unraisable("a chiplet's read of external memory");
            return false;
        } catch (...) {
            return false;
        }
    }

    bool write(std::uint64_t addr, const std::uint8_t* in,
               std::size_t bytes) noexcept override {
        pybind11::gil_scoped_acquire gil;
        try {
            writer_(addr, pybind11::bytes(
                reinterpret_cast<const char*>(in), bytes));
            return true;
        } catch (pybind11::error_already_set& error) {
            error.discard_as_unraisable(
                "a chiplet's write of external memory");
            return false;
        } catch (...) {
            return false;
        }
    }

private:
    pybind11::object reader_;
    pybind11::object writer_;
    std::uint64_t base_;
    std::uint64_t size_;
};

// A chiplet whose side of the link Python drives.
struct ManualChipletState {
    const mp64_chiplet_host* host = nullptr;
    std::vector<std::uint64_t> identity;
    bool accept_doorbells = true;
    bool attached = false;
    bool released = false;
    int stop_req = 0;
    int resets = 0;
    int steps = 0;
    // Each release: the boot address, the boot length, the boot argument.
    std::vector<std::tuple<std::uint64_t, std::uint64_t, std::uint64_t>>
        releases;
    std::vector<int> doorbells;
    std::vector<std::uint8_t> blob;     // what save yields and restore takes
};

inline mp64_chiplet_module make_manual_module(
        const std::shared_ptr<ManualChipletState>& state) {
    using Shared = std::shared_ptr<ManualChipletState>;
    mp64_chiplet_module module{};
    module.abi = MP64_CHIPLET_ABI;
    module.self = new Shared(state);
    module.bind = [](void* self, const mp64_chiplet_host* host) {
        (*static_cast<Shared*>(self))->host = host;
        (*static_cast<Shared*>(self))->attached = true;
    };
    module.identity = [](void* self, std::uint8_t index) -> std::uint64_t {
        const auto& words = (*static_cast<Shared*>(self))->identity;
        return index < words.size() ? words[index] : 0;
    };
    module.reset = [](void* self) {
        auto& state = **static_cast<Shared*>(self);
        state.released = false;
        state.resets += 1;
    };
    module.release = [](void* self, std::uint64_t addr, std::uint64_t len,
                        std::uint64_t arg) {
        auto& state = **static_cast<Shared*>(self);
        state.released = true;
        state.releases.emplace_back(addr, len, arg);
    };
    module.stop = [](void* self, int stop_req) {
        (*static_cast<Shared*>(self))->stop_req = stop_req;
    };
    module.doorbell = [](void* self, std::uint8_t src) -> int {
        auto& state = **static_cast<Shared*>(self);
        if (!state.accept_doorbells)
            return 0;
        state.doorbells.push_back(src);
        return 1;
    };
    module.step = [](void* self, std::uint32_t) -> int {
        (*static_cast<Shared*>(self))->steps += 1;
        return 0;
    };
    module.save = [](void* self, std::uint8_t* buffer,
                     std::size_t capacity) -> std::size_t {
        const auto& blob = (*static_cast<Shared*>(self))->blob;
        if (buffer != nullptr && capacity >= blob.size())
            std::memcpy(buffer, blob.data(), blob.size());
        return blob.size();
    };
    module.restore = [](void* self, const std::uint8_t* buffer,
                        std::size_t length) -> int {
        auto& state = **static_cast<Shared*>(self);
        state.blob.assign(buffer, buffer + length);
        return 0;
    };
    module.destroy = [](void* self) {
        auto* shared = static_cast<Shared*>(self);
        (*shared)->host = nullptr;
        (*shared)->attached = false;
        delete shared;
    };
    return module;
}

// MegaPad's side of the link with no machine around it.
class ChipletLink {
public:
    // `buffer` is the same object as `memory` when the memory is a buffer,
    // and null otherwise; it serves the tests' error injection.
    ChipletLink(std::unique_ptr<ExternalMemory> memory, BufferMemory* buffer,
                int core_count, int port_count, int range_count)
        : memory_(std::move(memory)),
          buffer_(buffer),
          table_(core_count, port_count, core_count),
          mailbox_(&table_),
          window_(port_count, core_count, range_count, memory_.get(),
                  &mailbox_) {}

    std::unique_ptr<ExternalMemory> memory_;
    BufferMemory* buffer_;
    MailboxPending table_;
    TableMailbox mailbox_;
    ChipletWindow window_;
};

inline void check_link_shape(int core_count, int port_count,
                             int range_count) {
    if (core_count < 1 || port_count < 0 || core_count + port_count > 256)
        throw pybind11::value_error(
            "cores and ports must share the 8-bit ID space");
    if (range_count < 1 || range_count > 255)
        throw pybind11::value_error("range_count must be 1 to 255");
}

inline void register_bindings(pybind11::module_& module) {
    namespace py = pybind11;

    py::class_<MailboxPending>(module, "MailboxPending", py::module_local())
        .def(py::init([](int core_count, int port_count,
                         py::object first_port_id) {
                 if (core_count < 1 || core_count > 256)
                     throw py::value_error("core_count must be 1 to 256");
                 if (port_count < 0 || port_count > 256)
                     throw py::value_error("port_count must be 0 to 256");
                 int first = core_count;
                 if (!first_port_id.is_none()) {
                     first = first_port_id.cast<int>();
                     if (first < core_count)
                         throw py::value_error(
                             "first_port_id must be at or above core_count");
                 }
                 if (port_count > 0 && first + port_count > 256)
                     throw py::value_error("port IDs must fit in 8 bits");
                 return MailboxPending(core_count, port_count, first);
             }),
             py::arg("core_count"), py::arg("port_count") = 0,
             py::arg("first_port_id") = py::none())
        .def_property_readonly("core_count", &MailboxPending::core_count)
        .def_property_readonly("port_count", &MailboxPending::port_count)
        .def_property_readonly("first_port_id",
                               &MailboxPending::first_port_id)
        .def("port_id", &MailboxPending::port_id, py::arg("port"))
        .def("is_participant", &MailboxPending::is_participant,
             py::arg("id"))
        .def("send", &MailboxPending::send,
             py::arg("source_id"), py::arg("target_id"))
        .def("acknowledge", &MailboxPending::acknowledge,
             py::arg("target_id"), py::arg("source_id"))
        .def("is_pending", &MailboxPending::is_pending,
             py::arg("target_id"), py::arg("source_id"))
        .def("any_pending", &MailboxPending::any_pending,
             py::arg("target_id"))
        .def("port_pending", &MailboxPending::port_pending,
             py::arg("target_id"))
        .def("lowest_source",
             [](const MailboxPending& table, int target_id) -> py::object {
                 const int source = table.lowest_source(target_id);
                 if (source == MailboxPending::no_source)
                     return py::none();
                 return py::int_(source);
             },
             py::arg("target_id"))
        .def("pending_mask", &MailboxPending::pending_mask,
             py::arg("target_id"))
        .def("clear_target", &MailboxPending::clear_target,
             py::arg("target_id"))
        .def("clear", &MailboxPending::clear)
        .def("words", [](const MailboxPending& table) {
                 return table.words();
             })
        .def("restore_words", &MailboxPending::restore_words,
             py::arg("words"));

    using Manual = std::shared_ptr<ManualChipletState>;
    const auto manual_host = [](const Manual& state) {
        if (state->host == nullptr)
            throw std::runtime_error(
                "the manual chiplet is attached to no port");
        return state->host;
    };
    py::class_<ManualChipletState, Manual>(
            module, "ManualChiplet", py::module_local())
        .def(py::init([](std::vector<std::uint64_t> identity) {
                 auto state = std::make_shared<ManualChipletState>();
                 state->identity = std::move(identity);
                 return state;
             }),
             py::arg("identity") = std::vector<std::uint64_t>{})
        .def_readwrite("accept_doorbells",
                       &ManualChipletState::accept_doorbells)
        .def_readwrite("blob", &ManualChipletState::blob)
        .def_readonly("attached", &ManualChipletState::attached)
        .def_readonly("released", &ManualChipletState::released)
        .def_readonly("stop_req", &ManualChipletState::stop_req)
        .def_readonly("resets", &ManualChipletState::resets)
        .def_readonly("steps", &ManualChipletState::steps)
        .def_readonly("releases", &ManualChipletState::releases)
        .def_readonly("doorbells", &ManualChipletState::doorbells)
        // One memory request as the chiplet. `data` gives the words of a
        // write. Returns (code, words done, the words read).
        .def("request",
             [manual_host](const Manual& state, std::uint64_t addr,
                           unsigned size, unsigned len, bool wen,
                           std::vector<std::uint64_t> data) {
                 const mp64_chiplet_host* host = manual_host(state);
                 data.resize(static_cast<std::size_t>(
                     len > 255 ? 256 : len + 1), 0);
                 unsigned done = 0;
                 const int code = host->mem(
                     host->ctx, addr, size, len, wen ? 1 : 0, data.data(),
                     &done);
                 return py::make_tuple(code, done, data);
             },
             py::arg("addr"), py::arg("size") = 3, py::arg("len") = 0,
             py::arg("wen") = false,
             py::arg("data") = std::vector<std::uint64_t>{})
        .def("ring",
             [manual_host](const Manual& state, int dst) {
                 const mp64_chiplet_host* host = manual_host(state);
                 host->doorbell(host->ctx, static_cast<std::uint8_t>(dst));
             },
             py::arg("dst"))
        .def("set_status",
             [manual_host](const Manual& state, bool ready, bool stopped) {
                 const mp64_chiplet_host* host = manual_host(state);
                 host->status(host->ctx, ready ? 1 : 0, stopped ? 1 : 0);
             },
             py::arg("ready"), py::arg("stopped") = false);

    const auto link_port = [](ChipletLink& link, int port) -> ChipletPort& {
        ChipletPort* found = link.window_.port(port);
        if (found == nullptr)
            throw py::index_error("the link has no such port");
        return *found;
    };
    const auto window_offset = [](unsigned offset) {
        if (offset >= ChipletWindow::SIZE || (offset & 7) != 0)
            throw py::value_error(
                "the window takes aligned 64-bit accesses below 0x100");
        return offset;
    };
    py::class_<ChipletLink>(module, "ChipletLink", py::module_local())
        .def(py::init([](py::buffer memory, std::uint64_t memory_base,
                         int core_count, int port_count, int range_count) {
                 check_link_shape(core_count, port_count, range_count);
                 auto buffer = std::make_unique<BufferMemory>(
                     std::move(memory), memory_base);
                 BufferMemory* raw = buffer.get();
                 return std::make_unique<ChipletLink>(
                     std::move(buffer), raw, core_count, port_count,
                     range_count);
             }),
             py::arg("memory"), py::arg("memory_base"),
             py::arg("core_count") = 4, py::arg("port_count") = 1,
             py::arg("range_count") = 8)
        // The same link over memory that two callables read and write.
        .def_static(
            "over_callbacks",
            [](py::object reader, py::object writer,
               std::uint64_t memory_base, std::uint64_t memory_size,
               int core_count, int port_count, int range_count) {
                check_link_shape(core_count, port_count, range_count);
                return std::make_unique<ChipletLink>(
                    std::make_unique<CallbackMemory>(
                        std::move(reader), std::move(writer), memory_base,
                        memory_size),
                    nullptr, core_count, port_count, range_count);
            },
            py::arg("reader"), py::arg("writer"), py::arg("memory_base"),
            py::arg("memory_size"), py::arg("core_count") = 4,
            py::arg("port_count") = 1, py::arg("range_count") = 8)
        .def_property_readonly(
            "mailbox",
            [](ChipletLink& link) -> MailboxPending& { return link.table_; },
            py::return_value_policy::reference_internal)
        .def_property_readonly("port_count", [](const ChipletLink& link) {
                 return link.window_.port_count();
             })
        .def_property_readonly("port_count_word",
                               [](const ChipletLink& link) {
                 return link.window_.port_count_word();
             })
        .def("read64",
             [window_offset](const ChipletLink& link, unsigned offset) {
                 return link.window_.read64(window_offset(offset));
             },
             py::arg("offset"))
        .def("write64",
             [window_offset](ChipletLink& link, unsigned offset,
                             std::uint64_t value) {
                 link.window_.write64(window_offset(offset), value);
             },
             py::arg("offset"), py::arg("value"))
        .def("attach_manual_chiplet",
             [link_port](ChipletLink& link, int port, const Manual& state) {
                 if (state->attached)
                     throw py::value_error(
                         "the manual chiplet is attached already");
                 const mp64_chiplet_module made = make_manual_module(state);
                 if (!link_port(link, port).attach(made)) {
                     made.destroy(made.self);
                     throw std::runtime_error("the port refused the module");
                 }
             },
             py::arg("port"), py::arg("chiplet"))
        .def("attach_test_chiplet",
             [link_port](ChipletLink& link, int port) {
                 mp64_chiplet_module made{};
                 if (create_test_chiplet(nullptr, &made) != 0)
                     throw std::runtime_error("no test chiplet was made");
                 if (!link_port(link, port).attach(made)) {
                     made.destroy(made.self);
                     throw std::runtime_error("the port refused the module");
                 }
             },
             py::arg("port"))
        .def("load_chiplet_module",
             [link_port](ChipletLink& link, int port,
                         const std::string& path, py::object config) {
                 const std::string text =
                     config.is_none() ? std::string() : config.cast<std::string>();
                 mp64_chiplet_module made{};
                 std::string error;
                 if (!load_module(path.c_str(),
                                  config.is_none() ? nullptr : text.c_str(),
                                  &made, &error))
                     throw std::runtime_error(error);
                 if (!link_port(link, port).attach(made)) {
                     made.destroy(made.self);
                     throw std::runtime_error("the port refused the module");
                 }
             },
             py::arg("port"), py::arg("path"),
             py::arg("config") = py::none())
        .def("detach",
             [link_port](ChipletLink& link, int port) {
                 link_port(link, port).detach();
             },
             py::arg("port"))
        .def("set_budget",
             [link_port](ChipletLink& link, int port, std::uint32_t budget) {
                 link_port(link, port);
                 link.window_.set_budget(port, budget);
             },
             py::arg("port"), py::arg("budget"))
        .def("step", [](ChipletLink& link) { return link.window_.step(); })
        .def("work_waits", [](const ChipletLink& link) {
                 return link.window_.work_waits();
             })
        .def("fail_memory_at",
             [](ChipletLink& link, py::object addr) {
                 if (link.buffer_ == nullptr)
                     throw std::runtime_error(
                         "error injection needs a link over a buffer");
                 link.buffer_->fail_at(
                     addr.is_none()
                         ? std::optional<std::uint64_t>()
                         : std::optional<std::uint64_t>(
                               addr.cast<std::uint64_t>()));
             },
             py::arg("addr"))
        .def("save",
             [](const ChipletLink& link) {
                 const std::vector<std::uint8_t> state = link.window_.save();
                 return py::bytes(
                     reinterpret_cast<const char*>(state.data()),
                     state.size());
             })
        .def("restore",
             [](ChipletLink& link, const py::bytes& state) {
                 const std::string text = state;
                 return link.window_.restore(std::vector<std::uint8_t>(
                     text.begin(), text.end()));
             },
             py::arg("state"));
}

}  // namespace megapad::chiplet
