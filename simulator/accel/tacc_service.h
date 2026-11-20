#pragma once

// The hosted profile's full-width tile accumulator: core 0's private TACC
// (docs/tile-engine.md, "Full-Width Tile Accumulator") and the BIOS words
// that reach it (docs/BIOS-DICTIONARY.md, "Full-width TACC" and "Borrowed
// tile work"). The tile registers stay with the hosted tile service, whose
// words pass TMODE, TSRC0, TSRC1 and TDST in; the bank, its ownership and
// format, every check the machine makes and the memory traffic are here.
// TAMAC's lanes come from shared/accel/tile_values, as in the emulator's
// native engine, and a STORE into a chiplet port's window goes to the
// shared port model (shared/accel/chiplet_port.h).
//
// An operation either completes or reports the one fault the machine would
// raise, an illegal operation, an alignment fault or a bus fault, before
// any change the machine would not make. A completed operation counts the
// TACC instructions it retired, for PERF-TILEOPS. The profile runs in
// supervisor mode without an MPU, so no privilege fault arises, and every
// operation completes inside its call, so BUSY and FORCE_PENDING read 0.
#include <pybind11/pybind11.h>

#include <array>
#include <cstdint>
#include <utility>

#include "../../shared/accel/chiplet_module.h"
#include "../../shared/accel/chiplet_port.h"
#include "../../shared/accel/tile_values.h"
#include "guest_memory.h"

namespace megapad::tacc_service {

namespace py = pybind11;
namespace tv = megapad::tile_values;
using Cell = std::uint64_t;

inline constexpr unsigned owner_none = 31;
inline constexpr Cell tile_bytes = 64;
inline constexpr Cell image_bytes = tv::TACC_IMAGE_BYTES;
inline constexpr Cell mmio_base = 0xFFFF'FF00'0000'0000ULL;
inline constexpr Cell mmio_limit = 0xFFFF'FF80'0000'0000ULL;

// The statuses of TACC-SEND and TACC-RELAY, the checked image words, as
// bios.asm gives them; docs/BIOS-DICTIONARY.md describes each. Having no
// external-memory transport, the simulator never returns 7.
inline constexpr Cell status_ok = 0;
inline constexpr Cell status_not_owner = 1;
inline constexpr Cell status_invalid = 2;
inline constexpr Cell status_format = 3;
inline constexpr Cell status_misaligned = 4;
inline constexpr Cell status_no_route = 5;
inline constexpr Cell status_refused = 6;
inline constexpr Cell status_transport = 7;

enum class Fault : unsigned { none = 0, illegal = 1, alignment = 2, bus = 3 };

// What made a bus fault, for its report.
enum class Cause : unsigned {
    none = 0,
    wraps = 1,       // the span runs past the end of the address space
    mmio = 2,        // it meets the device window
    crosses = 3,     // it starts in a region and runs past its end
    unrouted = 4,    // it starts in no region and, for a store, no window
    window_end = 5,  // a store image runs past its port's window
    refused = 6,     // the port or its chiplet did not take a store image
    scalar = 7,      // a TAMAC-SCALARS scalar lies outside ordinary memory
};

struct Outcome {
    Fault fault = Fault::none;
    Cause cause = Cause::none;
    Cell address = 0;      // where an alignment or bus fault is reported
    Cell length = 0;       // the bytes of the span that faulted
    Cell retired = 0;      // the TACC instructions this call completed
    int answer = 0;        // the chiplet's answer, for Cause::refused
};

// The width in bytes of one TAMAC-SCALARS scalar for each TMODE element
// width: the lane width of a defined format, and one byte for a reserved
// code, whose TAMAC traps after that byte is read. bios.asm keeps the same
// table (tamac_scalar_widths).
inline constexpr std::array<unsigned, 16> scalar_widths = {
    1, 2, 4, 8, 2, 2, 4, 8, 1, 1, 1, 1, 1, 1, 1, 1,
};

class Service {
public:
    // MEMORY is the profile's ordinary memory, held for OWNER, its Python
    // address space. WINDOWS, when the profile has chiplet ports, routes a
    // STORE image into memory a chiplet lends; LINK keeps them alive.
    Service(megapad::guest::GuestMemory memory, py::object owner,
            megapad::chiplet::ChipletWindow* windows, py::object link,
            Cell core = 0)
        : memory_(std::move(memory)), memory_owner_(std::move(owner)),
          link_(std::move(link)), windows_(windows), core_(core) {}

    // --- state ---------------------------------------------------------
    // TACC_STATUS as the profile's one core reads it.
    Cell status() const noexcept {
        const bool claimed = owner_ != owner_none;
        return Cell{claimed} | (Cell{claimed && owner_ == core_} << 1) |
               (Cell{valid_} << 2) | (Cell{dirty_} << 3) |
               (Cell{format_ew_ & 0xFu} << 5) | (Cell{format_signed_} << 9) |
               (Cell{owner_ & 0x1Fu} << 16);
    }
    const std::array<std::uint8_t, image_bytes>& bank() const noexcept { return bank_; }
    unsigned owner() const noexcept { return owner_; }
    bool valid() const noexcept { return valid_; }
    bool dirty() const noexcept { return dirty_; }
    unsigned format_ew() const noexcept { return format_ew_; }
    bool format_signed() const noexcept { return format_signed_; }
    const py::object& memory_owner() const noexcept { return memory_owner_; }

    // --- the lifecycle -----------------------------------------------------
    // TACC.TRY: claim a free bank; a held one, the caller's or another's,
    // stays as it is.
    Outcome try_claim() noexcept {
        if (owner_ == owner_none) owner_ = static_cast<unsigned>(core_);
        return retired(1);
    }

    // TACC.CLEAR: latch TMODE's format and zero the bank.
    Outcome clear(Cell tmode) noexcept {
        int ew = 0;
        bool signed_lanes = false;
        if (!mine() || !format_of(tmode, ew, signed_lanes)) return illegal();
        bank_.fill(0);
        valid_ = true;
        dirty_ = true;
        format_ew_ = static_cast<unsigned>(ew);
        format_signed_ = signed_lanes;
        return retired(1);
    }

    // TACC.LOAD: the canonical image at SOURCE in TMODE's format.
    Outcome load(Cell tmode, Cell source) {
        int ew = 0;
        bool signed_lanes = false;
        if (!mine() || !format_of(tmode, ew, signed_lanes)) return illegal();
        if (source & (tile_bytes - 1)) return alignment(source, image_bytes);
        const Route route = route_span(source, image_bytes, false);
        if (route.fault.fault != Fault::none) return route.fault;
        std::array<std::uint8_t, image_bytes> staged{};
        memory_.read(source, staged.data(), image_bytes);
        const std::size_t active = tv::tacc_active_bytes(ew);
        std::fill(staged.begin() + static_cast<std::ptrdiff_t>(active), staged.end(), 0);
        bank_ = staged;
        valid_ = true;
        dirty_ = false;
        format_ew_ = static_cast<unsigned>(ew);
        format_signed_ = signed_lanes;
        return retired(1);
    }

    // TACC.STORE: the canonical image to DESTINATION, in memory or in a
    // chiplet port's window. A store the port or its chiplet does not take
    // is a bus fault at the image base that leaves TACC as it was.
    Outcome store(Cell destination) {
        if (!mine() || !valid_) return illegal();
        if (destination & (tile_bytes - 1)) return alignment(destination, image_bytes);
        const Route route = route_span(destination, image_bytes, true);
        if (route.fault.fault != Fault::none) return route.fault;
        std::array<std::uint8_t, image_bytes> image = bank_;
        const std::size_t active = tv::tacc_active_bytes(static_cast<int>(format_ew_));
        std::fill(image.begin() + static_cast<std::ptrdiff_t>(active), image.end(), 0);
        if (route.link) {
            // The active units cross as one write and the chiplet writes
            // zeros over the rest of the 256 bytes.
            const unsigned units = static_cast<unsigned>(active / tile_bytes);
            const unsigned zeros = static_cast<unsigned>(image_bytes / tile_bytes) - units;
            const int answer = windows_->port(route.port)->lent_write(
                route.chiplet_addr, image.data(), units, zeros);
            if (answer != MP64_CHIPLET_OK) {
                Outcome refused = bus(Cause::refused, destination, image_bytes);
                refused.answer = answer;
                return refused;
            }
        } else {
            memory_.write(destination, image.data(), image_bytes);
        }
        dirty_ = false;
        return retired(1);
    }

    // TACC.RELEASE: zero the bank and give it up.
    Outcome release() noexcept {
        if (!mine()) return illegal();
        wipe();
        return retired(1);
    }

    // --- TAMAC ----------------------------------------------------------
    // The tile-by-tile form, E1 06: the lanes of the rows at A and B.
    Outcome tamac(Cell tmode, Cell a, Cell b) {
        int ew = 0;
        if (!tamac_admitted(tmode, ew)) return illegal();
        // Every source's alignment comes before any routing check.
        if (a & (tile_bytes - 1)) return alignment(a, tile_bytes);
        if (b & (tile_bytes - 1)) return alignment(b, tile_bytes);
        for (const Cell source : {a, b}) {
            const Route route = route_span(source, tile_bytes, false);
            if (route.fault.fault != Fault::none) return route.fault;
        }
        std::array<std::uint8_t, tile_bytes> left{};
        std::array<std::uint8_t, tile_bytes> right{};
        memory_.read(a, left.data(), tile_bytes);
        memory_.read(b, right.data(), tile_bytes);
        return accumulate(ew, left.data(), right.data());
    }

    // The broadcast form, E5 06 Rn: the lanes of the row at A, each times
    // the low lane-width bits of SCALAR.
    Outcome tamac_scalar(Cell tmode, Cell a, Cell scalar) {
        int ew = 0;
        if (!tamac_admitted(tmode, ew)) return illegal();
        if (a & (tile_bytes - 1)) return alignment(a, tile_bytes);
        const Route route = route_span(a, tile_bytes, false);
        if (route.fault.fault != Fault::none) return route.fault;
        std::array<std::uint8_t, tile_bytes> left{};
        memory_.read(a, left.data(), tile_bytes);
        std::array<std::uint8_t, tile_bytes> splat{};
        const int width = tv::TILE_FORMATS[ew].lane_bytes;
        for (int offset = 0; offset < static_cast<int>(tile_bytes); offset += width)
            for (int byte = 0; byte < width; ++byte)
                splat[static_cast<std::size_t>(offset + byte)] =
                    static_cast<std::uint8_t>(scalar >> (8 * byte));
        return accumulate(ew, left.data(), splat.data());
    }

    // TAMAC-SCALARS ( tiles xs n -- ): the broadcast form over COUNT tiles
    // from TILES, the i-th times the i-th scalar packed at TMODE's lane
    // width from SCALARS. As in the BIOS loop, each step sets TSRC0 to its
    // tile, reads its scalar, then accumulates, so SOURCE0 ends at the last
    // tile reached, faulting or not, and keeps its value when COUNT is 0.
    Outcome tamac_scalars(Cell tmode, Cell tiles, Cell scalars, Cell count, Cell& source0) {
        const unsigned width = scalar_widths[tmode & 0xF];
        Cell done = 0;
        for (Cell index = 0; index < count; ++index) {
            source0 = tiles + index * tile_bytes;
            const Cell at = scalars + index * width;
            Cell base = 0;
            Cell size = 0;
            if (at > ~Cell{0} - (width - 1) || !memory_.region_at(at, base, size) ||
                width > size - (at - base)) {
                Outcome fault = bus(Cause::scalar, at, width);
                fault.retired = done;
                return fault;
            }
            std::uint8_t bytes[8] = {};
            memory_.read(at, bytes, width);
            Cell scalar = 0;
            for (unsigned byte = 0; byte < width; ++byte)
                scalar |= Cell{bytes[byte]} << (8 * byte);
            Outcome step = tamac_scalar(tmode, source0, scalar);
            if (step.fault != Fault::none) {
                step.retired = done;
                return step;
            }
            ++done;
        }
        return retired(done);
    }

    // --- The checked image words ------------------------------------------
    // Each returns a status for every failure and never faults, checking
    // everything before its first instruction in the order bios.asm does,
    // so statuses 1 to 5 change nothing.

    // TACC-SEND ( dst -- status ): TACC.STORE of what the bank holds.
    Outcome send(Cell destination, Cell& status) {
        if (!mine()) return refused_before(status, status_not_owner);
        if (!valid_) return refused_before(status, status_invalid);
        if (destination & (tile_bytes - 1))
            return refused_before(status, status_misaligned);
        const Route to = route_span(destination, image_bytes, true);
        if (to.fault.fault != Fault::none)
            return refused_before(status, status_no_route);
        const Outcome stored = store(destination);
        if (stored.fault != Fault::none) {
            status = to.link ? status_refused : status_transport;
            return retired(0);
        }
        status = status_ok;
        return stored;
    }

    // TACC-RELAY ( src dst -- status ): TACC.LOAD from SOURCE in TMODE's
    // format, then TACC.STORE to DESTINATION. A failed store leaves the
    // loaded image in the bank, valid and clean.
    Outcome relay(Cell tmode, Cell source, Cell destination, Cell& status) {
        int ew = 0;
        bool signed_lanes = false;
        if (!mine()) return refused_before(status, status_not_owner);
        if (!format_of(tmode, ew, signed_lanes))
            return refused_before(status, status_format);
        if ((source | destination) & (tile_bytes - 1))
            return refused_before(status, status_misaligned);
        if (route_span(source, image_bytes, false).fault.fault != Fault::none)
            return refused_before(status, status_no_route);
        const Route to = route_span(destination, image_bytes, true);
        if (to.fault.fault != Fault::none)
            return refused_before(status, status_no_route);
        if (load(tmode, source).fault != Fault::none) {
            status = status_transport;
            return retired(0);
        }
        if (store(destination).fault != Fault::none) {
            status = to.link ? status_refused : status_transport;
            return retired(1);
        }
        status = status_ok;
        return retired(2);
    }

    // Cycle collection: the service owns its memory's pages and leases, the
    // address space they belong to, and the chiplet link.
    int gc_traverse(visitproc visit, void* arg) const noexcept {
        Py_VISIT(memory_owner_.ptr());
        Py_VISIT(link_.ptr());
        return memory_.gc_traverse(visit, arg);
    }
    void gc_clear() noexcept {
        windows_ = nullptr;
        py::object link = std::move(link_);
        py::object owner = std::move(memory_owner_);
        memory_.gc_clear();
    }

private:
    struct Route {
        Outcome fault;
        bool link = false;
        int port = -1;
        Cell chiplet_addr = 0;
    };

    bool mine() const noexcept { return owner_ == core_; }

    // TMODE's element width and integer signedness, when they name a TACC
    // format. Saturation and rounding are not part of it.
    static bool format_of(Cell tmode, int& ew, bool& signed_lanes) noexcept {
        ew = static_cast<int>(tmode & 0xF);
        if (!tv::tacc_format_is_legal(ew)) return false;
        signed_lanes = !tv::TILE_FORMATS[ew].is_float() && ((tmode >> 4) & 1);
        return true;
    }

    // TAMAC needs an owned, valid bank whose latched format TMODE names.
    bool tamac_admitted(Cell tmode, int& ew) const noexcept {
        bool signed_lanes = false;
        if (!mine() || !valid_ || !format_of(tmode, ew, signed_lanes)) return false;
        return static_cast<unsigned>(ew) == format_ew_ && signed_lanes == format_signed_;
    }

    Outcome accumulate(int ew, const std::uint8_t* a, const std::uint8_t* b) {
        tv::tacc_accumulate(ew, format_signed_, bank_.data(), a, b);
        dirty_ = true;
        return retired(1);
    }

    // Where the machine's TACC memory port takes a span, in the order its
    // checks run: past the address space, the device window, an ordinary
    // region it must stay inside, and for a STORE image (LINK_OK) a chiplet
    // port's window, which it must stay inside too.
    Route route_span(Cell start, Cell size, bool link_ok) const {
        Route route;
        const unsigned __int128 end = static_cast<unsigned __int128>(start) + size;
        if (end > (static_cast<unsigned __int128>(1) << 64)) {
            route.fault = bus(Cause::wraps, 0, size);
            return route;
        }
        if (start < mmio_limit && end > mmio_base) {
            route.fault = bus(Cause::mmio, start < mmio_base ? mmio_base : start, size);
            return route;
        }
        Cell base = 0;
        Cell region_size = 0;
        if (memory_.region_at(start, base, region_size)) {
            const unsigned __int128 region_end =
                static_cast<unsigned __int128>(base) + region_size;
            if (end > region_end)
                route.fault = bus(Cause::crosses, static_cast<Cell>(region_end), size);
            return route;
        }
        if (link_ok && windows_ != nullptr) {
            const auto window = windows_->route(start, size);
            if (window.in_window) {
                if (window.port < 0) {
                    route.fault = bus(Cause::window_end, window.first_forbidden, size);
                } else {
                    route.link = true;
                    route.port = window.port;
                    route.chiplet_addr = window.chiplet_addr;
                }
                return route;
            }
        }
        route.fault = bus(Cause::unrouted, start, size);
        return route;
    }

    void wipe() noexcept {
        bank_.fill(0);
        owner_ = owner_none;
        valid_ = false;
        dirty_ = false;
        format_ew_ = 0;
        format_signed_ = false;
    }

    static Outcome retired(Cell count) noexcept {
        Outcome outcome;
        outcome.retired = count;
        return outcome;
    }
    // A checked word's failure before its first instruction.
    static Outcome refused_before(Cell& status, Cell value) noexcept {
        status = value;
        return retired(0);
    }
    static Outcome illegal() noexcept {
        Outcome outcome;
        outcome.fault = Fault::illegal;
        return outcome;
    }
    static Outcome alignment(Cell address, Cell length) noexcept {
        Outcome outcome;
        outcome.fault = Fault::alignment;
        outcome.address = address;
        outcome.length = length;
        return outcome;
    }
    static Outcome bus(Cause cause, Cell address, Cell length) noexcept {
        Outcome outcome;
        outcome.fault = Fault::bus;
        outcome.cause = cause;
        outcome.address = address;
        outcome.length = length;
        return outcome;
    }

    megapad::guest::GuestMemory memory_;
    py::object memory_owner_;
    py::object link_;
    megapad::chiplet::ChipletWindow* windows_ = nullptr;
    Cell core_ = 0;

    std::array<std::uint8_t, image_bytes> bank_{};
    unsigned owner_ = owner_none;
    bool valid_ = false;
    bool dirty_ = false;
    unsigned format_ew_ = 0;
    bool format_signed_ = false;
};

}  // namespace megapad::tacc_service
