#pragma once

// A device on MegaPad's MMIO bus, as a backend's attachment table holds it.
//
// Every access names its requester: a core by its ID, or a DMA requester
// by a negative number. A table offers the device each whole access first,
// as a preflight that admits it or refuses it with a reason, and then moves
// its bytes one at a time, lowest address first, as the hosted simulator's
// contract asks of MMIO (docs/simulator-contract.md, section 3). A device
// may keep state per requester, and it drives each core's request lines,
// which the machine's scheduler reads.
//
// Offsets count from the MMIO aperture's base, 0xFFFF_FF00_0000_0000.
//
// The interface has no Python dependency.

#include <cstdint>
#include <stdexcept>

namespace megapad::mmio {

using Offset = std::uint64_t;
// A core's ID, 0 to 255, or a negative number for a DMA requester.
using Requester = int;

// The lines a device drives for one core.
enum Line : unsigned {
    LINE_IPI = 1u << 0,            // an interrupt request from the mailbox
    LINE_EXTERNAL_FLAG = 1u << 1,  // a chiplet port's signal, for BR.EF
    LINE_WAKE = 1u << 2,           // a request that ends IDL
};

// A whole access admitted, or refused with a reason that names the rule.
struct Verdict {
    const char* refusal = nullptr;  // static text; null when admitted

    bool admitted() const noexcept { return refusal == nullptr; }
    static Verdict admit() noexcept { return Verdict{}; }
    static Verdict refuse(const char* reason) noexcept { return Verdict{reason}; }
};

// A byte that reached a device outside what its preflight admits, as when a
// caller skips the preflight.
class Refused : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

class Device {
public:
    virtual ~Device() = default;

    // Admit or refuse one whole access of `width` bytes at `offset` before
    // any byte moves.
    virtual Verdict preflight(Offset offset, unsigned width, bool write,
                              Requester requester) = 0;
    virtual std::uint8_t read8(Offset offset, Requester requester) = 0;
    virtual void write8(Offset offset, std::uint8_t value,
                        Requester requester) = 0;
    // The Line bits this device raises for `core`.
    virtual unsigned lines(int core) const noexcept {
        static_cast<void>(core);
        return 0;
    }
};

// The widths of MegaPad's scalar loads and stores.
inline bool scalar_width(unsigned width) noexcept {
    return width == 1 || width == 2 || width == 4 || width == 8;
}

// Whether [offset, offset + width) lies inside [start, limit).
inline bool inside(Offset offset, unsigned width, Offset start,
                   Offset limit) noexcept {
    return offset >= start && offset < limit && width <= limit - offset;
}

}  // namespace megapad::mmio
