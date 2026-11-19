#pragma once

#include <algorithm>
#include <cstdint>
#include <limits>

namespace mp64::machine {

// Raw guest-visible memory apertures. Python buffer ownership, mutation
// exclusion, and attachment capacity remain outside this hot routing view.
struct GuestMemoryMap {
    uint8_t* mem = nullptr;
    uint64_t mem_size = 0;

    uint8_t* hbw_mem = nullptr;
    uint64_t hbw_base = 0;
    uint64_t hbw_size = 0;

    uint8_t* ext_mem = nullptr;
    uint64_t ext_mem_base = 0;
    uint64_t ext_mem_size = 0;

    uint8_t* vram_mem = nullptr;
    uint64_t vram_base = 0;
    uint64_t vram_size = 0;
};

enum class MemoryAccessPolicy : uint8_t {
    // Natural-width operations and instruction/stack traffic use the
    // historical VRAM -> external -> HBW -> Bank 0 routing order.
    SCALAR,

    // Supervisor byte operations use the distinct historical
    // HBW -> external -> VRAM -> Bank 0 routing order.
    SUPERVISOR_BYTE,
};

enum class MemoryRegionKind : uint8_t {
    NONE,
    VRAM,
    EXTERNAL,
    HBW,
    BANK0,
};

struct ResolvedMemorySpan {
    uint8_t* data = nullptr;
    uint64_t available = 0;
    MemoryRegionKind region = MemoryRegionKind::NONE;
    uint8_t priority = 0;
    MemoryAccessPolicy policy = MemoryAccessPolicy::SCALAR;

    explicit operator bool() const noexcept {
        return data != nullptr;
    }

    bool covers(uint64_t size) const noexcept {
        return data != nullptr && size <= available;
    }
};

inline bool region_contains(
        uint64_t base,
        uint64_t size,
        uint64_t address) noexcept {
    // Subtraction after the lower-bound check avoids wrapping base + size.
    return address >= base && address - base < size;
}

inline bool region_span_fits(
        uint64_t size,
        uint64_t offset,
        uint64_t span) noexcept {
    return offset < size && span <= size - offset;
}

namespace detail {

inline ResolvedMemorySpan explicit_region(
        uint8_t* data,
        uint64_t base,
        uint64_t size,
        uint64_t address,
        MemoryRegionKind region,
        uint8_t priority,
        MemoryAccessPolicy policy) noexcept {
    if (!data || !region_contains(base, size, address))
        return {};
    const uint64_t offset = address - base;
    return {
        data + offset,
        size - offset,
        region,
        priority,
        policy,
    };
}

inline void cap_before_higher_priority_region(
        uint64_t address,
        uint8_t* region_data,
        uint64_t region_base,
        uint64_t region_size,
        uint64_t& available) noexcept {
    if (
        region_data &&
        region_size != 0 &&
        region_base > address
    ) {
        available = std::min(
            available,
            region_base - address);
    }
}

inline void cap_at_guest_wrap(
        uint64_t address,
        uint64_t& available) noexcept {
    const uint64_t through_max =
        std::numeric_limits<uint64_t>::max() - address;
    if (address != 0 && available - 1 > through_max)
        available = through_max + 1;
}

}  // namespace detail

// Resolve the maximum directly contiguous extent from one guest address.
// The result selects a region once, stops before guest-address wrap, and
// caps the extent at the first higher-priority aperture. An address in no
// region resolves to nothing: it lies outside the memory map, and the
// caller faults the access. Callers retain MMIO, privilege, and
// bus-transaction policy.
inline ResolvedMemorySpan resolve_memory_span(
        const GuestMemoryMap& memory,
        uint64_t address,
        MemoryAccessPolicy policy) noexcept {
    ResolvedMemorySpan resolved;
    if (policy == MemoryAccessPolicy::SCALAR) {
        resolved = detail::explicit_region(
            memory.vram_mem,
            memory.vram_base,
            memory.vram_size,
            address,
            MemoryRegionKind::VRAM,
            0,
            policy);
        if (!resolved) {
            resolved = detail::explicit_region(
                memory.ext_mem,
                memory.ext_mem_base,
                memory.ext_mem_size,
                address,
                MemoryRegionKind::EXTERNAL,
                1,
                policy);
        }
        if (!resolved) {
            resolved = detail::explicit_region(
                memory.hbw_mem,
                memory.hbw_base,
                memory.hbw_size,
                address,
                MemoryRegionKind::HBW,
                2,
                policy);
        }
    } else {
        resolved = detail::explicit_region(
            memory.hbw_mem,
            memory.hbw_base,
            memory.hbw_size,
            address,
            MemoryRegionKind::HBW,
            0,
            policy);
        if (!resolved) {
            resolved = detail::explicit_region(
                memory.ext_mem,
                memory.ext_mem_base,
                memory.ext_mem_size,
                address,
                MemoryRegionKind::EXTERNAL,
                1,
                policy);
        }
        if (!resolved) {
            resolved = detail::explicit_region(
                memory.vram_mem,
                memory.vram_base,
                memory.vram_size,
                address,
                MemoryRegionKind::VRAM,
                2,
                policy);
        }
    }

    if (!resolved) {
        resolved = detail::explicit_region(
            memory.mem,
            0,
            memory.mem_size,
            address,
            MemoryRegionKind::BANK0,
            3,
            policy);
    }
    if (!resolved)
        return {};

    detail::cap_at_guest_wrap(address, resolved.available);
    if (policy == MemoryAccessPolicy::SCALAR) {
        if (resolved.priority > 0) {
            detail::cap_before_higher_priority_region(
                address,
                memory.vram_mem,
                memory.vram_base,
                memory.vram_size,
                resolved.available);
        }
        if (resolved.priority > 1) {
            detail::cap_before_higher_priority_region(
                address,
                memory.ext_mem,
                memory.ext_mem_base,
                memory.ext_mem_size,
                resolved.available);
        }
        if (resolved.priority > 2) {
            detail::cap_before_higher_priority_region(
                address,
                memory.hbw_mem,
                memory.hbw_base,
                memory.hbw_size,
                resolved.available);
        }
    } else {
        if (resolved.priority > 0) {
            detail::cap_before_higher_priority_region(
                address,
                memory.hbw_mem,
                memory.hbw_base,
                memory.hbw_size,
                resolved.available);
        }
        if (resolved.priority > 1) {
            detail::cap_before_higher_priority_region(
                address,
                memory.ext_mem,
                memory.ext_mem_base,
                memory.ext_mem_size,
                resolved.available);
        }
        if (resolved.priority > 2) {
            detail::cap_before_higher_priority_region(
                address,
                memory.vram_mem,
                memory.vram_base,
                memory.vram_size,
                resolved.available);
        }
    }
    return resolved;
}

}  // namespace mp64::machine
