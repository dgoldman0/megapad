#pragma once

// The hosted profile's storage service: one storage unit's medium and the
// BIOS-visible controller state, and the checked transfer and flush that
// the BIOS's disk words make of a controller's commands.  It is the one
// definition of the simulator's disk words: the Python words call it
// through its binding and the native engine calls it directly.
//
// A transfer completes when the guest makes it.  It is validated whole, as
// the BIOS validates it, then moved in commands of at most 255 sectors, as
// the BIOS splits it; each command takes the test faults and is admitted to
// the request observer as the emulator's controller takes and admits one
// (shared/accel/storage_faults.h).  The medium is a Python bytearray or
// writable mapping the service holds a buffer lease on, and the host file
// that holds it is shared.storage's HostImageFile, the emulator's own.
#include <pybind11/pybind11.h>

#include <algorithm>
#include <cstdint>
#include <cstring>
#include <memory>
#include <optional>
#include <stdexcept>
#include <utility>

#include "../../shared/accel/storage_faults.h"
#include "guest_memory.h"

namespace megapad::storage_service {

namespace py = pybind11;
using Cell = std::uint64_t;
using megapad::storage::Fault;
using megapad::storage::FaultQueue;
using megapad::storage::RequestObserver;

inline constexpr Cell sector_size = 512;
inline constexpr Cell default_image_sectors = 2048;
inline constexpr Cell max_media_sectors = 0xFFFFFFFFull;
inline constexpr Cell controller_max_sectors = 255;

inline constexpr Cell command_read = 0x01;
inline constexpr Cell command_write = 0x02;
inline constexpr Cell command_flush = 0xFF;

inline constexpr Cell status_error = 0x02;
inline constexpr Cell status_result_valid = 0x08;
inline constexpr Cell status_media_changed = 0x10;
inline constexpr Cell status_write_protected = 0x20;
inline constexpr Cell status_present = 0x80;

inline constexpr Cell result_ok = 0x00;
inline constexpr Cell result_no_media = 0x01;
inline constexpr Cell result_unsupported = 0x02;
inline constexpr Cell result_invalid_count = 0x03;
inline constexpr Cell result_lba_range = 0x04;
inline constexpr Cell result_address_overflow = 0x05;
inline constexpr Cell result_dma_invalid = 0x06;
inline constexpr Cell result_dma_failure = 0x07;
inline constexpr Cell result_write_protected = 0x08;
inline constexpr Cell result_media_removed = 0x0B;
inline constexpr Cell result_flush_failure = 0x0D;
inline constexpr Cell result_internal = 0x0E;
inline constexpr Cell result_partial = 0x80;

inline constexpr Cell cap_read = 0x01;
inline constexpr Cell cap_write = 0x02;
inline constexpr Cell cap_flush = 0x04;
inline constexpr Cell cap_precise_result = 0x08;
inline constexpr Cell cap_completion = 0x10;
inline constexpr Cell cap_media_gen = 0x20;
inline constexpr Cell cap_gen_guard = 0x40;
inline constexpr Cell all_caps = cap_read | cap_write | cap_flush | cap_precise_result
    | cap_completion | cap_media_gen | cap_gen_guard;
inline constexpr Cell checked_caps = cap_precise_result | cap_completion
    | cap_media_gen | cap_gen_guard;
inline constexpr Cell read_required_caps = cap_read | checked_caps;
inline constexpr Cell write_required_caps = cap_write | checked_caps;
inline constexpr Cell flush_required_caps = cap_flush | checked_caps;

struct Outcome {
    Cell completed = 0;
    Cell status = result_ok;
};

class Service {
public:
    explicit Service(Cell capabilities) : capabilities_(capabilities) {
        host_file_ = py::module_::import("shared.storage").attr("HostImageFile")();
        image_ = py::reinterpret_steal<py::object>(PyByteArray_FromStringAndSize("", 0));
        if (!image_) throw py::error_already_set();
    }

    Service(Service&&) = default;
    virtual ~Service() = default;

    // A test seam called just before a command's guarded acceptance, where
    // a test can swap the medium; a Python subclass overrides it.
    virtual void before_guarded_accept(Cell command, Cell expected_generation) {
        (void)command;
        (void)expected_generation;
    }

    // One runtime owns a service at a time.
    void claim() {
        if (claimed_) throw py::value_error("storage service is already owned by a runtime");
        claimed_ = true;
    }

    // --- registers -------------------------------------------------------
    Cell capabilities() const noexcept { return capabilities_; }
    bool present() const noexcept { return present_; }
    bool write_protected() const noexcept { return write_protected_; }
    Cell media_generation() const noexcept { return media_generation_; }
    Cell completion() const noexcept { return completion_; }
    Cell result() const noexcept { return result_; }
    Cell transferred() const noexcept { return transferred_; }
    Cell total_sectors() const noexcept { return present_ ? size_ / sector_size : 0; }
    Cell status() const noexcept {
        Cell status = 0;
        if (present_) status |= status_present;
        if (write_protected_) status |= status_write_protected;
        if (media_changed_) status |= status_media_changed;
        if (result_valid_) {
            status |= status_result_valid;
            if (result_ & 0x7F) status |= status_error;
        }
        return status;
    }

    // --- media -----------------------------------------------------------
    // IMAGE becomes the medium: a bytearray or writable mapping the service
    // keeps, already checked by the caller.  The host file holds it when
    // FILE_HOLDS_MEDIA says so.
    void attach(py::object image, py::object image_path, bool write_protected,
                bool file_holds_media) {
        lease(std::move(image));
        image_path_ = std::move(image_path);
        host_file_.attr("media_attached")(py::arg("file_holds_media") = file_holds_media);
        present_ = true;
        write_protected_ = write_protected;
        advance_media_generation();
    }

    void detach() {
        py::object empty = py::reinterpret_steal<py::object>(PyByteArray_FromStringAndSize("", 0));
        if (!empty) throw py::error_already_set();
        lease(std::move(empty));
        image_path_ = py::none();
        host_file_.attr("media_detached")();
        present_ = false;
        write_protected_ = false;
        advance_media_generation();
    }

    void set_write_protected(bool enabled) {
        if (!present_ && enabled)
            throw py::value_error("absent storage cannot be write protected");
        write_protected_ = enabled;
    }

    void acknowledge_media_change() noexcept { media_changed_ = false; }
    const py::object& image() const noexcept { return image_; }
    const py::object& image_path() const noexcept { return image_path_; }
    const py::object& host_file() const noexcept { return host_file_; }
    const std::uint8_t* data() const noexcept { return data_; }
    Cell size() const noexcept { return size_; }

    // --- test seams ----------------------------------------------------------
    FaultQueue& faults() noexcept { return faults_; }
    RequestObserver* observer() noexcept { return observer_; }
    // OBSERVER, a Python-owned RequestObserver, records what this service
    // admits from now on, and HOOK, if not None, is called at admission.
    void observe(py::object observer, py::object hook) {
        RequestObserver* recorder = &observer.cast<RequestObserver&>();
        py::object previous = std::move(observer_object_);
        py::object previous_hook = std::move(admission_hook_);
        observer_object_ = std::move(observer);
        observer_ = recorder;
        admission_hook_ = std::move(hook);
    }
    void stop_observing() {
        observer_ = nullptr;
        py::object previous = std::move(observer_object_);
        py::object hook = std::move(admission_hook_);
        observer_object_ = py::none();
        admission_hook_ = py::none();
    }
    // Whether an admission calls back into Python, which keeps the service's
    // requests out of the native engine.
    bool hooked() const noexcept { return !admission_hook_.is_none(); }

    // --- guest memory ----------------------------------------------------
    // The memory of the runtime that owns this service, OWNER, which the
    // native engine's disk words move data through.
    void bind_memory(py::object owner, megapad::guest::GuestMemory memory) {
        memory_ = std::move(memory);
        py::object previous = std::move(memory_owner_);
        memory_owner_ = std::move(owner);
    }
    const py::object& memory_owner() const noexcept { return memory_owner_; }
    megapad::guest::GuestMemory& memory() noexcept { return memory_; }

    // --- the checked words -------------------------------------------------
    // A READ or WRITE of COUNT sectors from LBA through the span at DMA of
    // MEMORY, bound to GENERATION when one is given.
    Outcome transfer(megapad::guest::GuestMemory& memory, bool write, Cell dma, Cell lba,
                     Cell count, std::optional<Cell> generation) {
        const Cell expected = generation ? *generation : media_generation_;
        const Cell checked = validate(memory, write, dma, lba, count, generation);
        if (checked != result_ok) return {0, checked};
        const Cell command = write ? command_write : command_read;
        Cell completed = 0;
        while (completed < count) {
            if (media_generation_ != expected)
                return {completed, partial_if(result_media_removed, completed)};
            const Cell chunk = std::min(controller_max_sectors, count - completed);
            const Cell chunk_dma = dma + completed * sector_size;
            const Cell media_start = (lba + completed) * sector_size;
            before_guarded_accept(command, expected);
            if (media_generation_ != expected) {
                publish(result_media_removed, 0);
                return {completed, partial_if(result_media_removed, completed)};
            }
            if (std::optional<Fault> fault = take_early_fault(command)) {
                const Cell result = act(*fault, false);
                publish(result, 0);
                return chunk_status(result, 0, chunk, completed);
            }
            if (write && write_protected_) {
                publish(result_write_protected, 0);
                return {completed, partial_if(result_write_protected, completed)};
            }
            if (observer_) admit(command, lba + completed, chunk);
            std::optional<megapad::storage::TransferFault> hit;
            if (!faults_.empty())
                hit = faults_.take_in_transfer(static_cast<std::uint8_t>(command), chunk);
            // A fault stops the command where the byte-serial controller
            // would have met it, with what it had moved by then.
            const Cell moved = hit ? hit->moved : chunk * sector_size;
            if (!move(memory, write, chunk_dma, media_start, lba + completed, moved)) {
                publish(result_dma_failure, 0);
                return {completed, partial_if(result_dma_failure, completed)};
            }
            if (hit) {
                const Cell result = act(hit->fault, moved > 0);
                publish(result, hit->confirmed);
                const Outcome outcome = chunk_status(result, hit->confirmed, chunk, completed);
                if (outcome.status != result_ok) return outcome;
                completed = outcome.completed;
                continue;
            }
            completed += chunk;
            if (media_generation_ != expected)
                return {completed, result_media_removed | result_partial};
            publish(result_ok, chunk);
        }
        return {completed, result_ok};
    }

    // A FLUSH, bound to GENERATION when one is given.
    Cell flush(std::optional<Cell> generation) {
        const Cell expected = generation ? *generation : media_generation_;
        Cell status = result_ok;
        if (!present_) status = result_no_media;
        else if ((capabilities_ & flush_required_caps) != flush_required_caps)
            status = result_unsupported;
        else if (generation && *generation != media_generation_)
            status = result_media_removed;
        if (status != result_ok) return status;
        before_guarded_accept(command_flush, expected);
        if (media_generation_ != expected) {
            publish(result_media_removed, 0);
            return result_media_removed;
        }
        std::optional<Fault> fault = take_early_fault(command_flush);
        if (!fault) {
            if (observer_) admit(command_flush, 0, 0);
            if (!faults_.empty())
                fault = faults_.take(megapad::storage::Stage::flush,
                                     static_cast<std::uint8_t>(command_flush),
                                     std::nullopt, std::nullopt);
        }
        if (fault) {
            // A flush the fault stops leaves the host file as it was.
            const Cell result = act(*fault, false);
            publish(result, 0);
            return result;
        }
        if (!image_path_.is_none()) {
            try {
                host_file_.attr("flush")(image_path_, image_);
            } catch (py::error_already_set& error) {
                if (!error.matches(PyExc_OSError)) throw;
                const Cell result = result_flush_failure | result_partial;
                publish(result, 0);
                return result;
            }
        }
        if (media_generation_ != expected)
            return result_media_removed | result_partial;
        publish(result_ok, 0);
        return result_ok;
    }

    // Cycle collection: the service owns its medium, host file, hooks and
    // the memory it was bound to.
    int gc_traverse(visitproc visit, void* arg) const noexcept {
        Py_VISIT(image_.ptr());
        Py_VISIT(image_path_.ptr());
        Py_VISIT(host_file_.ptr());
        Py_VISIT(admission_hook_.ptr());
        Py_VISIT(observer_object_.ptr());
        Py_VISIT(memory_owner_.ptr());
        return memory_.gc_traverse(visit, arg);
    }
    void gc_clear() noexcept {
        lease_.reset();
        data_ = nullptr;
        size_ = 0;
        py::object image = std::move(image_);
        py::object path = std::move(image_path_);
        py::object hook = std::move(admission_hook_);
        observer_ = nullptr;
        py::object recorder = std::move(observer_object_);
        py::object owner = std::move(memory_owner_);
        memory_.gc_clear();
        present_ = false;
    }

private:
    static Cell partial_if(Cell result, Cell completed) noexcept {
        return completed ? result | result_partial : result;
    }

    // The BIOS's checked transfer after one controller command: the sectors
    // it confirmed join those completed, and a nonzero result, or an OK that
    // confirmed fewer sectors than asked (which the BIOS reports as
    // INTERNAL), ends the transfer, PARTIAL when any sector of it completed.
    static Outcome chunk_status(Cell result, Cell transferred, Cell chunk,
                                Cell completed) noexcept {
        completed += transferred;
        Cell status;
        if (result != result_ok) status = result;
        else if (transferred != chunk) status = result_internal;
        else return {completed, result_ok};
        return {completed, partial_if(status, completed)};
    }

    void lease(py::object image) {
        lease_.reset();
        data_ = nullptr;
        size_ = 0;
        PyObject* raw_view = PyMemoryView_FromObject(image.ptr());
        if (raw_view == nullptr) throw py::error_already_set();
        auto view = py::reinterpret_steal<py::buffer>(raw_view);
        auto info = std::make_unique<py::buffer_info>(view.request(true));
        if (info->readonly || info->ndim != 1 || info->itemsize != 1)
            throw py::value_error("a storage medium is a writable buffer of bytes");
        size_ = static_cast<Cell>(info->size);
        data_ = static_cast<std::uint8_t*>(info->ptr);
        lease_ = std::move(info);
        py::object previous = std::move(image_);
        image_ = std::move(image);
    }

    void advance_media_generation() noexcept {
        media_generation_ = (media_generation_ + 1) & 0xFFFFFFFFull;
        media_changed_ = true;
    }

    void publish(Cell result, Cell transferred) noexcept {
        result_ = result & 0xFF;
        transferred_ = transferred & 0xFF;
        result_valid_ = true;
        completion_ = (completion_ + 1) & 0xFFFFFFFFull;
    }

    Cell validate(const megapad::guest::GuestMemory& memory, bool write, Cell dma,
                  Cell lba, Cell count, std::optional<Cell> generation) {
        if (!present_) return result_no_media;
        const Cell required = write ? write_required_caps : read_required_caps;
        if ((capabilities_ & required) != required) return result_unsupported;
        if (generation && *generation != media_generation_) return result_media_removed;
        if (count == 0) return result_invalid_count;
        const Cell capacity = total_sectors();
        if (lba >= capacity || count > capacity - lba) return result_lba_range;
        // A count within the u32 capacity shifted by nine cannot overflow.
        const Cell bytes = count * sector_size;
        if (dma != 0 && bytes - 1 > ~Cell{0} - dma) return result_address_overflow;
        if (!memory.contains(dma, bytes)) return result_dma_invalid;
        return result_ok;
    }

    std::optional<Fault> take_early_fault(Cell command) {
        if (faults_.empty()) return std::nullopt;
        const auto code = static_cast<std::uint8_t>(command);
        std::optional<Fault> fault = faults_.take(megapad::storage::Stage::accept, code,
                                                  std::nullopt, std::nullopt);
        if (!fault)
            fault = faults_.take(megapad::storage::Stage::start, code,
                                 std::nullopt, std::nullopt);
        return fault;
    }

    // Act on FAULT and return the controller's result for it.
    Cell act(const Fault& fault, bool effect_applied) {
        if (fault.action == megapad::storage::Action::detach) detach();
        return megapad::storage::fault_result(fault, effect_applied);
    }

    void admit(Cell command, Cell lba, Cell count) {
        observer_->admit(static_cast<std::uint8_t>(command), lba, count, faults_);
        if (!admission_hook_.is_none()) admission_hook_(command, lba, count);
    }

    // Move BYTES between the medium at MEDIA_START and MEMORY at DMA, marking
    // the sectors a write changes from FIRST_SECTOR on.
    bool move(megapad::guest::GuestMemory& memory, bool write, Cell dma, Cell media_start,
              Cell first_sector, Cell bytes) {
        if (bytes == 0) return true;
        try {
            if (write) {
                host_file_.attr("mark")(first_sector, (bytes + sector_size - 1) / sector_size);
                memory.read(dma, data_ + media_start, bytes);
            } else {
                memory.write(dma, data_ + media_start, bytes);
            }
        } catch (const std::runtime_error&) {
            return false;
        }
        return true;
    }

    Cell capabilities_;
    py::object image_;
    std::unique_ptr<py::buffer_info> lease_;
    std::uint8_t* data_ = nullptr;
    Cell size_ = 0;
    py::object image_path_ = py::none();
    py::object host_file_;
    bool present_ = false;
    bool write_protected_ = false;
    bool media_changed_ = false;
    bool result_valid_ = false;
    Cell media_generation_ = 0;
    Cell completion_ = 0;
    Cell result_ = result_ok;
    Cell transferred_ = 0;
    bool claimed_ = false;
    FaultQueue faults_;
    RequestObserver* observer_ = nullptr;
    py::object observer_object_ = py::none();
    py::object admission_hook_ = py::none();
    py::object memory_owner_ = py::none();
    megapad::guest::GuestMemory memory_;
};

// A Python subclass of the service, which may override the test seam.
class PythonService : public Service {
public:
    using Service::Service;
    explicit PythonService(Service&& service) : Service(std::move(service)) {}
    void before_guarded_accept(Cell command, Cell expected_generation) override {
        PYBIND11_OVERRIDE_NAME(void, Service, "_before_guarded_accept",
                               before_guarded_accept, command, expected_generation);
    }
};

}  // namespace megapad::storage_service
