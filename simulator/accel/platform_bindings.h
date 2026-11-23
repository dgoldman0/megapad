#pragma once

// The hosted simulator's MMIO devices and the table they attach to.
//
// A device claims its windows when it attaches to the PlatformTable, which
// refuses a window that overlaps another, and every access reaches the
// device whose window holds its first byte, with the table's requester: a
// whole-access preflight first, then each byte lowest address first. An
// access outside every window is refused. Devices written in C++ implement
// mmio::Device; a Python object with the methods preflight(offset, width,
// *, write), read8(offset) and write8(offset, value) attaches through a thin
// adapter and keeps its own model. A refusal reaches Python through the
// handler the platform installs, which raises the device's own error type.
//
// The C++ devices here are System Info, built from the machine's topology
// and memory geometry; the chiplet port window over the shared port model;
// the mailbox, the shared model in shared/accel/mailbox.h, over the table
// its ports ring through; and the spinlock bank of
// shared/accel/spinlock_bank.h, whose window waits for the crypto guard's
// lock 8 to become the bank's (docs/simulator-contract.md, section 6).

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include <algorithm>
#include <array>
#include <cstdint>
#include <limits>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "../../shared/accel/chiplet_bindings.h"
#include "../../shared/accel/mailbox.h"
#include "../../shared/accel/mmio_device.h"
#include "../../shared/accel/spinlock_bank.h"
#include "collected.h"
#include "core_scheduler.h"

namespace megapad::platform {
namespace py = pybind11;
using mmio::Offset;
using mmio::Requester;
using mmio::Verdict;

// The MMIO aperture is 2^39 bytes from its base.
constexpr Offset APERTURE = Offset{1} << 39;

// ---------------------------------------------------------------------------
//  System Info
// ---------------------------------------------------------------------------

// The machine a profile describes: one to four full cores, and zero to three
// clusters of four microcores, the shapes the emulator and the BIOS support.
struct Topology {
    int full_cores = 1;
    int clusters = 0;

    static constexpr int MAX_FULL_CORES = 4;
    static constexpr int MAX_CLUSTERS = 3;
    static constexpr int MICROCORES_PER_CLUSTER = 4;

    void validate() const {
        if (full_cores < 1 || full_cores > MAX_FULL_CORES)
            throw py::value_error("a machine has one to four full cores");
        if (clusters < 0 || clusters > MAX_CLUSTERS)
            throw py::value_error("a machine has zero to three clusters");
    }
    int core_count() const noexcept {
        return full_cores + clusters * MICROCORES_PER_CLUSTER;
    }
    // Full cores, clusters, and the three DMA requesters: NIC, disk, WOTS.
    int bus_ports() const noexcept { return full_cores + clusters + 3; }
    std::uint64_t cluster_enable() const noexcept {
        return (std::uint64_t{1} << clusters) - 1;
    }
};

// The read-only register file at 0x300: fifteen little-endian qwords.
class SysInfo final : public mmio::Device {
public:
    static constexpr Offset OFFSET = 0x300;
    static constexpr std::size_t REGISTERS = 15;
    static constexpr Offset SIZE = 8 * REGISTERS;
    static constexpr Offset LIMIT = OFFSET + SIZE;

    struct Geometry {
        std::uint64_t bank0_size = 0;
        std::uint64_t hbw_base = 0, hbw_size = 0;
        std::uint64_t external_base = 0, external_size = 0;
        std::uint64_t vram_base = 0, vram_size = 0;
    };

    SysInfo(std::uint64_t board_id, const Topology& topology,
            const Geometry& geometry, std::uint64_t crypto_capabilities,
            std::uint64_t chiplet_ports) {
        topology.validate();
        registers_ = {
            board_id,
            geometry.bank0_size,
            static_cast<std::uint64_t>(topology.core_count()),
            topology.cluster_enable(),
            geometry.hbw_base,
            geometry.hbw_size,
            geometry.bank0_size + geometry.hbw_size,
            geometry.external_base,
            geometry.external_size,
            static_cast<std::uint64_t>(topology.full_cores),
            geometry.vram_base,
            geometry.vram_size,
            crypto_capabilities,
            static_cast<std::uint64_t>(topology.bus_ports()),
            chiplet_ports,
        };
    }

    const std::array<std::uint64_t, REGISTERS>& registers() const noexcept {
        return registers_;
    }

    Verdict preflight(Offset offset, unsigned width, bool write,
                      Requester) override {
        if (!mmio::scalar_width(width))
            return Verdict::refuse("SysInfo width must be 1, 2, 4, or 8 bytes");
        if (!mmio::inside(offset, width, OFFSET, LIMIT))
            return Verdict::refuse("access is outside the exact SysInfo MMIO window");
        if (offset % width != 0)
            return Verdict::refuse("SysInfo access is not naturally aligned");
        if (write)
            return Verdict::refuse("the hosted SysInfo window is read-only");
        return Verdict::admit();
    }

    std::uint8_t read8(Offset offset, Requester) override {
        if (offset < OFFSET || offset >= LIMIT)
            throw mmio::Refused("byte access is outside the exact SysInfo MMIO window");
        const Offset local = offset - OFFSET;
        return static_cast<std::uint8_t>(registers_[local / 8] >> (8 * (local % 8)));
    }

    void write8(Offset offset, std::uint8_t, Requester) override {
        if (offset < OFFSET || offset >= LIMIT)
            throw mmio::Refused("byte access is outside the exact SysInfo MMIO window");
        throw mmio::Refused("the hosted SysInfo window is read-only");
    }

private:
    std::array<std::uint64_t, REGISTERS> registers_{};
};

// ---------------------------------------------------------------------------
//  The chiplet port window and the mailbox
// ---------------------------------------------------------------------------

// The requesters that take a signal at once when a chiplet raises it inside
// one of their own accesses, as a worker's IPI handler does on the machine:
// a bit for each such core. Core 0 keeps its signals pending until it
// acknowledges them or sleeps, and so does every core outside the mask.
struct SignalTakers {
    std::uint64_t takers = 0;

    // A chiplet stepped inside an access by `requester`. A taker
    // acknowledges every port's signal; a full core's signal is a job,
    // which the worker takes at the start of its next turn, so it stays.
    void take(chiplet::MailboxPending& table, Requester requester) const {
        if (requester < 0 || requester >= 64 || ((takers >> requester) & 1) == 0)
            return;
        for (int port = 0; port < table.port_count(); ++port)
            table.acknowledge(requester, table.port_id(port));
    }
};

// The port window at 0xE00 over a ChipletLink. Its registers are 64 bits
// wide and take aligned 64-bit accesses alone. The chiplets step before the
// guest looks at a register, and a register written takes its eight bytes
// in order and changes with the last.
class ChipletWindowDevice final : public mmio::Device {
public:
    static constexpr Offset OFFSET = 0xE00;
    static constexpr Offset LIMIT = OFFSET + chiplet::ChipletWindow::SIZE;

    ChipletWindowDevice(py::object link, std::shared_ptr<SignalTakers> takers)
        : link_(&link.cast<chiplet::ChipletLink&>()), owner_(std::move(link)),
          takers_(std::move(takers)) {}

    Verdict preflight(Offset offset, unsigned width, bool, Requester) override {
        if (width != 8 || offset % 8 != 0 || !mmio::inside(offset, width, OFFSET, LIMIT))
            return Verdict::refuse("the chiplet window takes aligned 64-bit accesses");
        return Verdict::admit();
    }

    std::uint8_t read8(Offset offset, Requester requester) override {
        require_byte(offset);
        const unsigned index = static_cast<unsigned>(offset & 7);
        chiplet::ChipletLink& link = this->link();
        if (index == 0) {
            link.window_.step();
            if (takers_)
                takers_->take(link.table_, requester);
        }
        const std::uint64_t value =
            link.window_.read64(static_cast<unsigned>((offset & ~Offset{7}) - OFFSET));
        return static_cast<std::uint8_t>(value >> (8 * index));
    }

    void write8(Offset offset, std::uint8_t value, Requester) override {
        require_byte(offset);
        const unsigned index = static_cast<unsigned>(offset & 7);
        const Offset base = offset & ~Offset{7};
        if (index == 0) {
            latch_base_ = base;
        } else if (latch_base_ != base) {
            throw mmio::Refused("a chiplet register is written as one 64-bit access");
        }
        latch_[index] = value;
        if (index == 7) {
            latch_base_.reset();
            std::uint64_t word = 0;
            for (int byte = 7; byte >= 0; --byte)
                word = (word << 8) | latch_[static_cast<std::size_t>(byte)];
            link().window_.write64(static_cast<unsigned>(base - OFFSET), word);
        }
    }

    int gc_traverse(visitproc visit, void* arg) const noexcept {
        Py_VISIT(owner_.ptr());
        return 0;
    }
    void gc_clear() noexcept {
        link_ = nullptr;
        py::object owner = std::move(owner_);
    }

private:
    chiplet::ChipletLink& link() {
        if (link_ == nullptr)
            throw mmio::Refused("the chiplet window has no link");
        return *link_;
    }
    static void require_byte(Offset offset) {
        if (offset < OFFSET || offset >= LIMIT)
            throw mmio::Refused("byte access is outside the chiplet window");
    }

    chiplet::ChipletLink* link_;
    py::object owner_;
    std::shared_ptr<SignalTakers> takers_;
    std::array<std::uint8_t, 8> latch_{};
    std::optional<Offset> latch_base_;
};

// The shared mailbox over the table it is given: a ChipletLink's, whose
// ports then step before each read of a core's pending signals, through
// STATUS or `pending`, a MailboxPending's, or one of its own for a core
// count.
class HostedMailbox final : public mailbox::Mailbox {
public:
    explicit HostedMailbox(int core_count) : Mailbox(core_count) {}
    HostedMailbox(chiplet::MailboxPending* table, py::object owner)
        : Mailbox(table), owner_(std::move(owner)) {}

    static std::shared_ptr<HostedMailbox> over(
        py::object owner, std::shared_ptr<SignalTakers> takers = nullptr) {
        if (py::isinstance<chiplet::ChipletLink>(owner)) {
            auto& link = owner.cast<chiplet::ChipletLink&>();
            auto made = std::make_shared<HostedMailbox>(&link.table_, owner);
            // The ports step in read8, where the requester is known.
            made->stepped_ = &link;
            made->takers_ = std::move(takers);
            return made;
        }
        if (py::isinstance<chiplet::MailboxPending>(owner)) {
            auto& table = owner.cast<chiplet::MailboxPending&>();
            return std::make_shared<HostedMailbox>(&table, owner);
        }
        throw py::type_error(
            "a mailbox is made over a ChipletLink, a MailboxPending or a core count");
    }

    // A mailbox over a released owner's table has no table left, and every
    // access to it is refused.
    Verdict preflight(Offset offset, unsigned width, bool write,
                      Requester requester) override {
        if (detached_)
            return Verdict::refuse("the mailbox has no table");
        return Mailbox::preflight(offset, width, write, requester);
    }
    std::uint8_t read8(Offset offset, Requester requester) override {
        if (detached_)
            throw mmio::Refused("the mailbox has no table");
        if (offset == STATUS && table().is_core(requester))
            step_before_pending(requester);
        return Mailbox::read8(offset, requester);
    }
    // The requester's pending signals from every ID, the whole mask that
    // CSR MBOX reads on the machine, where STATUS shows IDs 0 to 7. A
    // requester that names no core reads 0.
    std::uint64_t pending(Requester requester) {
        if (detached_)
            throw mmio::Refused("the mailbox has no table");
        if (!table().is_core(requester))
            return 0;
        step_before_pending(requester);
        return table().pending_mask(requester);
    }
    void write8(Offset offset, std::uint8_t value, Requester requester) override {
        if (detached_)
            throw mmio::Refused("the mailbox has no table");
        Mailbox::write8(offset, value, requester);
    }
    unsigned lines(int core) const noexcept override {
        return detached_ ? 0 : Mailbox::lines(core);
    }

    int gc_traverse(visitproc visit, void* arg) const noexcept {
        Py_VISIT(owner_.ptr());
        return 0;
    }
    void gc_clear() noexcept {
        if (owner_)
            detached_ = true;
        stepped_ = nullptr;
        py::object owner = std::move(owner_);
    }

private:
    // The ports catch up before a core reads its pending signals, so a core
    // that polls for a port's signal makes progress, and a core among the
    // takers takes at once the port's signal that the step raised for it.
    void step_before_pending(Requester requester) {
        if (stepped_ == nullptr)
            return;
        stepped_->window_.step();
        if (takers_)
            takers_->take(stepped_->table_, requester);
    }

    py::object owner_;
    chiplet::ChipletLink* stepped_ = nullptr;
    std::shared_ptr<SignalTakers> takers_;
    bool detached_ = false;
};

// ---------------------------------------------------------------------------
//  The attachment table
// ---------------------------------------------------------------------------

// An int's value, or nothing for another type, a bool, or an int beyond
// 64 bits.
inline std::optional<long long> exact_integer(py::handle value) {
    if (py::isinstance<py::bool_>(value) || !py::isinstance<py::int_>(value))
        return std::nullopt;
    int overflow = 0;
    const long long exact = PyLong_AsLongLongAndOverflow(value.ptr(), &overflow);
    if (overflow != 0)
        return std::nullopt;
    if (exact == -1 && PyErr_Occurred())
        throw py::error_already_set();
    return exact;
}

// A Python object attached as a device. Its exceptions pass through.
class PythonDevice final : public mmio::Device {
public:
    explicit PythonDevice(py::object target) : target_(std::move(target)) {}

    const py::object& target() const noexcept { return target_; }
    py::object release() noexcept { return std::move(target_); }

    Verdict preflight(Offset offset, unsigned width, bool write, Requester) override {
        target_.attr("preflight")(offset, width, py::arg("write") = write);
        return Verdict::admit();
    }

    std::uint8_t read8(Offset offset, Requester) override {
        py::object value = target_.attr("read8")(offset);
        const std::optional<long long> byte = exact_integer(value);
        if (!byte || *byte < 0 || *byte > 0xFF)
            throw py::value_error("MMIO read8 did not return a byte");
        return static_cast<std::uint8_t>(*byte);
    }

    void write8(Offset offset, std::uint8_t value, Requester) override {
        target_.attr("write8")(offset, value);
    }

private:
    py::object target_;
};

class PlatformTable {
public:
    // Attach `device` under `name` with the half-open windows given. A
    // C++ device attaches as itself; any other object attaches through the
    // Python adapter.
    void attach(const std::string& name, py::object device,
                const std::vector<std::pair<Offset, Offset>>& windows) {
        if (find_attachment(name) != nullptr)
            throw py::value_error("a device named " + name + " is already attached");
        if (windows.empty())
            throw py::value_error("a device attaches with at least one window");
        for (std::size_t i = 0; i < windows.size(); ++i) {
            const auto [start, limit] = windows[i];
            if (start >= limit || limit > APERTURE)
                throw py::value_error("an MMIO window must be a nonempty span of the aperture");
            for (std::size_t j = 0; j < i; ++j)
                if (start < windows[j].second && windows[j].first < limit)
                    throw py::value_error("a device's MMIO windows overlap each other");
            for (const Window& window : windows_)
                if (start < window.limit && window.start < limit)
                    throw py::value_error(
                        "the MMIO window of " + name + " overlaps the window of " +
                        attachments_[window.attachment].name);
        }
        Attachment attachment{name, nullptr, py::object(), nullptr};
        bind(attachment, std::move(device));
        attachments_.push_back(std::move(attachment));
        const std::size_t index = attachments_.size() - 1;
        for (const auto& [start, limit] : windows)
            windows_.push_back(Window{start, limit, index});
        std::sort(windows_.begin(), windows_.end(),
                  [](const Window& a, const Window& b) { return a.start < b.start; });
    }

    // Put `device` in the place of the device attached as `name`, in the
    // same windows.
    void replace(const std::string& name, py::object device) {
        Attachment* attachment = find_attachment(name);
        if (attachment == nullptr)
            throw py::key_error("no device named " + name + " is attached");
        Attachment replaced{name, nullptr, py::object(), nullptr};
        bind(replaced, std::move(device));
        // Release the old references only once the new ones are in place.
        Attachment old = std::move(*attachment);
        *attachment = std::move(replaced);
    }

    // The object attached as `name`: the device's Python object.
    py::object device(const std::string& name) const {
        const Attachment* attachment = find_attachment(name);
        if (attachment == nullptr)
            throw py::key_error("no device named " + name + " is attached");
        return object_of(*attachment);
    }

    // The name of the device whose window holds `offset`, or None.
    py::object name_at(Offset offset) const {
        const Window* window = find_window(offset);
        if (window == nullptr)
            return py::none();
        return py::str(attachments_[window->attachment].name);
    }

    // The Python object an adapter at `offset` reaches, or None where a C++
    // device or no device answers.
    py::object python_target_at(Offset offset) const {
        const Window* window = find_window(offset);
        if (window == nullptr)
            return py::none();
        const Attachment& attachment = attachments_[window->attachment];
        if (attachment.python == nullptr)
            return py::none();
        return attachment.python->target();
    }

    // The windows in address order, as (start, limit, name).
    py::list windows() const {
        py::list listed;
        for (const Window& window : windows_)
            listed.append(py::make_tuple(window.start, window.limit,
                                         attachments_[window.attachment].name));
        return listed;
    }

    void preflight(Offset offset, unsigned width, bool write) {
        const Window* window = find_window(offset);
        if (window == nullptr)
            refuse("platform", "access is outside every admitted platform MMIO window",
                   offset, width, write);
        const Attachment& attachment = attachments_[window->attachment];
        const Verdict verdict =
            attachment.device->preflight(offset, width, write, requester_);
        if (!verdict.admitted())
            refuse(attachment.name, verdict.refusal, offset, width, write);
    }

    std::uint8_t read8(Offset offset) {
        const Attachment& attachment = byte_attachment(offset, false);
        try {
            return attachment.device->read8(offset, requester_);
        } catch (const mmio::Refused& refused) {
            refuse(attachment.name, refused.what(), offset, 1, false);
        }
    }

    void write8(Offset offset, std::uint8_t value) {
        const Attachment& attachment = byte_attachment(offset, true);
        try {
            attachment.device->write8(offset, value, requester_);
        } catch (const mmio::Refused& refused) {
            refuse(attachment.name, refused.what(), offset, 1, true);
        }
    }

    // The Line bits every device raises for `core`.
    unsigned lines(int core) const noexcept {
        unsigned raised = 0;
        for (const Attachment& attachment : attachments_)
            raised |= attachment.device->lines(core);
        return raised;
    }

    Requester requester() const noexcept { return requester_; }
    // A core's ID, 0 to 255, or a DMA requester's negative number.
    void set_requester(Requester requester) {
        if (requester > 255)
            throw py::value_error("a core's ID fits in 8 bits");
        requester_ = requester;
    }

    // `handler(name, message, offset, width, write)` raises the error a
    // refusal becomes in Python.
    void set_refusal(py::object handler) { refuse_ = std::move(handler); }

    int gc_traverse(visitproc visit, void* arg) const noexcept {
        for (const Attachment& attachment : attachments_) {
            Py_VISIT(attachment.object.ptr());
            if (attachment.python != nullptr)
                Py_VISIT(attachment.python->target().ptr());
        }
        Py_VISIT(refuse_.ptr());
        return 0;
    }

    // The table attaches nothing afterwards: every access is refused, and
    // with no handler the refusal is a ValueError.
    void gc_clear() noexcept {
        std::vector<Window> windows = std::move(windows_);
        windows_.clear();
        std::vector<Attachment> attachments = std::move(attachments_);
        attachments_.clear();
        py::object handler = std::move(refuse_);
    }

private:
    struct Attachment {
        std::string name;
        std::shared_ptr<mmio::Device> device;
        // A C++ device's Python object, which owns the device too.
        py::object object;
        // The adapter of a Python device, owned by `device`.
        PythonDevice* python;
    };
    struct Window {
        Offset start;
        Offset limit;
        std::size_t attachment;
    };

    static void bind(Attachment& attachment, py::object device) {
        if (py::isinstance<mmio::Device>(device)) {
            attachment.device = device.cast<std::shared_ptr<mmio::Device>>();
            attachment.object = std::move(device);
            attachment.python = nullptr;
            return;
        }
        auto adapter = std::make_shared<PythonDevice>(std::move(device));
        attachment.python = adapter.get();
        attachment.device = std::move(adapter);
        attachment.object = py::object();
    }

    static py::object object_of(const Attachment& attachment) {
        if (attachment.python != nullptr)
            return attachment.python->target();
        return attachment.object;
    }

    Attachment* find_attachment(const std::string& name) {
        for (Attachment& attachment : attachments_)
            if (attachment.name == name)
                return &attachment;
        return nullptr;
    }
    const Attachment* find_attachment(const std::string& name) const {
        for (const Attachment& attachment : attachments_)
            if (attachment.name == name)
                return &attachment;
        return nullptr;
    }

    const Window* find_window(Offset offset) const noexcept {
        auto after = std::upper_bound(
            windows_.begin(), windows_.end(), offset,
            [](Offset value, const Window& window) { return value < window.start; });
        if (after == windows_.begin())
            return nullptr;
        const Window& window = *(after - 1);
        return offset < window.limit ? &window : nullptr;
    }

    const Attachment& byte_attachment(Offset offset, bool write) {
        const Window* window = find_window(offset);
        if (window == nullptr)
            refuse("platform", "byte access is outside every admitted platform MMIO window",
                   offset, 1, write);
        return attachments_[window->attachment];
    }

    [[noreturn]] void refuse(const std::string& name, const char* message,
                             Offset offset, unsigned width, bool write) const {
        if (refuse_)
            refuse_(name, message, offset, width, write);
        throw py::value_error(message);
    }

    std::vector<Attachment> attachments_;
    std::vector<Window> windows_;
    py::object refuse_;
    Requester requester_ = 0;
};

// ---------------------------------------------------------------------------
//  Python
// ---------------------------------------------------------------------------

// An int argument's value; an int beyond 64 bits counts as out of range.
inline long long checked_index(py::handle value, const char* what) {
    if (py::isinstance<py::bool_>(value) || !py::isinstance<py::int_>(value))
        throw py::type_error(std::string(what) + " must be an integer");
    const std::optional<long long> exact = exact_integer(value);
    return exact ? *exact : std::numeric_limits<long long>::min();
}

inline int lock_index(py::handle lock_id) {
    const long long lock = checked_index(lock_id, "spinlock ID");
    if (lock < 0 || lock >= spinlock::SpinlockBank::LOCKS)
        throw py::value_error("spinlock ID is outside the hosted bank");
    return static_cast<int>(lock);
}

inline int requester_index(const spinlock::SpinlockBank& bank, py::handle requester_core) {
    const long long core = checked_index(requester_core, "spinlock requester core");
    if (core < 0 || core >= bank.core_count())
        throw py::value_error("spinlock requester is outside the hosted profile");
    return static_cast<int>(core);
}

inline void register_bindings(py::module_& module) {
    using spinlock::SpinlockBank;

    py::class_<mmio::Device, std::shared_ptr<mmio::Device>>(
        module, "MMIODevice", py::module_local())
        .def("lines", &mmio::Device::lines, py::arg("core"));

    module.attr("LINE_IPI") = py::int_(static_cast<unsigned>(mmio::LINE_IPI));
    module.attr("LINE_EXTERNAL_FLAG") =
        py::int_(static_cast<unsigned>(mmio::LINE_EXTERNAL_FLAG));
    module.attr("LINE_WAKE") = py::int_(static_cast<unsigned>(mmio::LINE_WAKE));

    py::class_<SysInfo, mmio::Device, std::shared_ptr<SysInfo>>(
        module, "SysInfo", py::module_local())
        .def(py::init([](std::uint64_t board_id, int full_cores, int clusters,
                         std::uint64_t bank0_size, std::uint64_t hbw_base,
                         std::uint64_t hbw_size, std::uint64_t external_base,
                         std::uint64_t external_size, std::uint64_t vram_base,
                         std::uint64_t vram_size, std::uint64_t crypto_capabilities,
                         std::uint64_t chiplet_ports) {
                 SysInfo::Geometry geometry;
                 geometry.bank0_size = bank0_size;
                 geometry.hbw_base = hbw_base;
                 geometry.hbw_size = hbw_size;
                 geometry.external_base = external_base;
                 geometry.external_size = external_size;
                 geometry.vram_base = vram_base;
                 geometry.vram_size = vram_size;
                 return std::make_shared<SysInfo>(
                     board_id, Topology{full_cores, clusters}, geometry,
                     crypto_capabilities, chiplet_ports);
             }),
             py::kw_only(), py::arg("board_id"), py::arg("full_cores"),
             py::arg("clusters"), py::arg("bank0_size"), py::arg("hbw_base"),
             py::arg("hbw_size"), py::arg("external_base"),
             py::arg("external_size"), py::arg("vram_base"), py::arg("vram_size"),
             py::arg("crypto_capabilities"), py::arg("chiplet_ports"))
        .def_property_readonly("registers", [](const SysInfo& info) {
            return py::tuple(py::cast(info.registers()));
        })
        .def_property_readonly_static("OFFSET", [](py::object) { return SysInfo::OFFSET; })
        .def_property_readonly_static("SIZE", [](py::object) { return SysInfo::SIZE; });

    py::class_<SignalTakers, std::shared_ptr<SignalTakers>>(
        module, "SignalTakers", py::module_local())
        .def(py::init<>())
        .def_readwrite("takers", &SignalTakers::takers);

    py::class_<ChipletWindowDevice, mmio::Device, std::shared_ptr<ChipletWindowDevice>>(
        module, "ChipletWindowDevice", py::module_local(),
        collected::setup<ChipletWindowDevice>())
        .def(py::init([](py::object link, std::shared_ptr<SignalTakers> takers) {
                 if (!py::isinstance<chiplet::ChipletLink>(link))
                     throw py::type_error("the chiplet window is made over a ChipletLink");
                 return std::make_shared<ChipletWindowDevice>(std::move(link),
                                                              std::move(takers));
             }),
             py::arg("link"), py::arg("takers") = nullptr);

    py::class_<HostedMailbox, mmio::Device, std::shared_ptr<HostedMailbox>>(
        module, "Mailbox", py::module_local(), collected::setup<HostedMailbox>())
        .def(py::init([](py::object over, std::shared_ptr<SignalTakers> takers)
                          -> std::shared_ptr<HostedMailbox> {
                 if (py::isinstance<py::int_>(over) && !py::isinstance<py::bool_>(over)) {
                     const int cores = over.cast<int>();
                     if (cores < 1 || cores > 256)
                         throw py::value_error("a mailbox serves 1 to 256 cores");
                     return std::make_shared<HostedMailbox>(cores);
                 }
                 return HostedMailbox::over(std::move(over), std::move(takers));
             }),
             py::arg("over"), py::arg("takers") = nullptr)
        .def_property_readonly(
            "table", [](HostedMailbox& mailbox) -> chiplet::MailboxPending& {
                return mailbox.table();
            },
            py::return_value_policy::reference_internal)
        // The members live in the shared model, which pybind11 does not
        // know as a class, so each takes the hosted mailbox it is bound to.
        .def_property_readonly("core_count",
                               [](const HostedMailbox& mailbox) { return mailbox.core_count(); })
        .def("data", [](const HostedMailbox& mailbox, int core) { return mailbox.data(core); },
             py::arg("core"))
        .def("pending", [](HostedMailbox& mailbox, int core) { return mailbox.pending(core); },
             py::arg("core"))
        .def("set_data",
             [](HostedMailbox& mailbox, int core, std::uint64_t value) {
                 mailbox.set_data(core, value);
             },
             py::arg("core"), py::arg("value"))
        .def("send",
             [](HostedMailbox& mailbox, int source, int target) {
                 return mailbox.send(source, target);
             },
             py::arg("source"), py::arg("target"))
        .def("reset", [](HostedMailbox& mailbox) { mailbox.reset(); });

    py::class_<SpinlockBank, mmio::Device, std::shared_ptr<SpinlockBank>>(
        module, "SpinlockBank", py::module_local())
        .def(py::init([](py::object core_count) {
                 const long long cores = checked_index(core_count, "spinlock core count");
                 if (cores <= 0)
                     throw py::value_error("spinlock core count must be positive");
                 if (cores > 256)
                     throw py::value_error("spinlock core count must fit the 8-bit core IDs");
                 return std::make_shared<SpinlockBank>(static_cast<int>(cores));
             }),
             py::kw_only(), py::arg("core_count"))
        .def_property_readonly("core_count", &SpinlockBank::core_count)
        .def_property_readonly("lock_count", [](const SpinlockBank&) {
            return SpinlockBank::LOCKS;
        })
        .def_property_readonly("owners", [](const SpinlockBank& bank) {
            py::tuple owners(SpinlockBank::LOCKS);
            for (int lock = 0; lock < SpinlockBank::LOCKS; ++lock) {
                const int owner = bank.owner(lock);
                owners[static_cast<std::size_t>(lock)] =
                    owner == SpinlockBank::FREE ? py::object(py::none()) : py::object(py::int_(owner));
            }
            return owners;
        })
        .def("owner",
             [](const SpinlockBank& bank, py::object lock_id) -> py::object {
                 const int lock = lock_index(lock_id);
                 const int owner = bank.owner(lock);
                 if (owner == SpinlockBank::FREE)
                     return py::none();
                 return py::int_(owner);
             },
             py::arg("lock_id"))
        .def("acquire",
             [](SpinlockBank& bank, py::object lock_id, py::object requester_core) {
                 const int lock = lock_index(lock_id);
                 const int core = requester_index(bank, requester_core);
                 return bank.acquire(lock, core);
             },
             py::arg("lock_id"), py::arg("requester_core"))
        .def("release",
             [](SpinlockBank& bank, py::object lock_id, py::object requester_core) {
                 const int lock = lock_index(lock_id);
                 const int core = requester_index(bank, requester_core);
                 bank.release(lock, core);
             },
             py::arg("lock_id"), py::arg("requester_core"))
        .def("reset", &SpinlockBank::reset)
        // The window's byte face, for its tests until a table attaches it.
        .def("preflight",
             [](SpinlockBank& bank, Offset offset, unsigned width, bool write,
                Requester requester) {
                 const Verdict verdict = bank.preflight(offset, width, write, requester);
                 if (!verdict.admitted())
                     throw py::value_error(verdict.refusal);
             },
             py::arg("offset"), py::arg("width"), py::kw_only(), py::arg("write"),
             py::arg("requester") = 0)
        .def("read8",
             [](SpinlockBank& bank, Offset offset, Requester requester) {
                 try {
                     return bank.read8(offset, requester);
                 } catch (const mmio::Refused& refused) {
                     throw py::value_error(refused.what());
                 }
             },
             py::arg("offset"), py::kw_only(), py::arg("requester") = 0)
        .def("write8",
             [](SpinlockBank& bank, Offset offset, std::uint8_t value, Requester requester) {
                 try {
                     bank.write8(offset, value, requester);
                 } catch (const mmio::Refused& refused) {
                     throw py::value_error(refused.what());
                 }
             },
             py::arg("offset"), py::arg("value"), py::kw_only(), py::arg("requester") = 0);

    py::class_<PlatformTable>(module, "PlatformTable", py::module_local(),
                              collected::setup<PlatformTable>())
        .def(py::init<>())
        .def("attach", &PlatformTable::attach, py::arg("name"), py::arg("device"),
             py::arg("windows"))
        .def("replace", &PlatformTable::replace, py::arg("name"), py::arg("device"))
        .def("device", &PlatformTable::device, py::arg("name"))
        .def("name_at", &PlatformTable::name_at, py::arg("offset"))
        .def("python_target_at", &PlatformTable::python_target_at, py::arg("offset"))
        .def_property_readonly("windows", &PlatformTable::windows)
        .def("preflight", &PlatformTable::preflight, py::arg("offset"), py::arg("width"),
             py::arg("write"))
        .def("read8", &PlatformTable::read8, py::arg("offset"))
        .def("write8", &PlatformTable::write8, py::arg("offset"), py::arg("value"))
        .def("lines", &PlatformTable::lines, py::arg("core"))
        .def_property("requester", &PlatformTable::requester, &PlatformTable::set_requester)
        .def("set_refusal", &PlatformTable::set_refusal, py::arg("handler"));

    namespace sched = megapad::scheduler;
    py::enum_<sched::State>(module, "CoreState", py::module_local())
        .value("AWAKE", sched::State::AWAKE)
        .value("ASLEEP", sched::State::ASLEEP)
        .value("IDLE", sched::State::IDLE);
    py::enum_<sched::TurnEnd>(module, "TurnEnd", py::module_local())
        .value("QUANTUM", sched::TurnEnd::QUANTUM)
        .value("SPIN", sched::TurnEnd::SPIN)
        .value("SLEEP", sched::TurnEnd::SLEEP)
        .value("DONE", sched::TurnEnd::DONE);
    // A deadline of None is no deadline.
    const auto deadline_in = [](std::optional<std::uint64_t> deadline) {
        return deadline ? *deadline : sched::CoreScheduler::NO_DEADLINE;
    };
    const auto deadline_out = [](std::uint64_t deadline) -> std::optional<std::uint64_t> {
        if (deadline == sched::CoreScheduler::NO_DEADLINE)
            return std::nullopt;
        return deadline;
    };
    py::class_<sched::CoreScheduler>(module, "CoreScheduler", py::module_local())
        .def(py::init<int, std::uint64_t>(), py::arg("cores"), py::arg("quantum"))
        .def_property_readonly("cores", &sched::CoreScheduler::cores)
        .def_property_readonly("quantum", &sched::CoreScheduler::quantum)
        .def_property_readonly("current", &sched::CoreScheduler::current)
        .def("state", &sched::CoreScheduler::state, py::arg("core"))
        .def("deadline",
             [deadline_out](const sched::CoreScheduler& scheduler, int core) {
                 return deadline_out(scheduler.deadline(core));
             },
             py::arg("core"))
        .def("steps", &sched::CoreScheduler::steps, py::arg("core"))
        .def("turns", &sched::CoreScheduler::turns, py::arg("core"))
        .def("awake", &sched::CoreScheduler::awake, py::arg("core"))
        .def("any_awake", &sched::CoreScheduler::any_awake)
        .def("begin_next_turn", &sched::CoreScheduler::begin_next_turn)
        .def_property_readonly("turn_left", &sched::CoreScheduler::turn_left)
        .def_property_readonly("turn_used", &sched::CoreScheduler::turn_used)
        .def("account", &sched::CoreScheduler::account, py::arg("steps"))
        .def("end_turn",
             [deadline_in](sched::CoreScheduler& scheduler, sched::TurnEnd reason,
                           std::optional<std::uint64_t> deadline) {
                 scheduler.end_turn(reason, deadline_in(deadline));
             },
             py::arg("reason"), py::arg("deadline") = py::none())
        .def_property_readonly("round_started", &sched::CoreScheduler::round_started)
        .def_property_readonly("round_length", &sched::CoreScheduler::round_length)
        .def("finish_round", &sched::CoreScheduler::finish_round)
        .def("wake", &sched::CoreScheduler::wake, py::arg("core"))
        .def("rest", &sched::CoreScheduler::rest, py::arg("core"))
        .def("wake_mask", &sched::CoreScheduler::wake_mask, py::arg("ipi_lines"),
             py::arg("terminal_input"), py::arg("now_ms"))
        .def("apply_wakes", &sched::CoreScheduler::apply_wakes, py::arg("ipi_lines"),
             py::arg("terminal_input"), py::arg("now_ms"))
        .def_property_readonly(
            "earliest_deadline",
            [deadline_out](const sched::CoreScheduler& scheduler) {
                return deadline_out(scheduler.earliest_deadline());
            });
}

}  // namespace megapad::platform
