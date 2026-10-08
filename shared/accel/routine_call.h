#pragma once

// The call interface between the native semantic executor and the hybrid
// routine runner, which live in separate extensions. The runner publishes one
// RoutineCall in a capsule; the executor calls it without Python.
#include <cstddef>
#include <cstdint>

namespace megapad::hybrid {

inline constexpr const char* ROUTINE_CALL_CAPSULE = "megapad.hybrid.routine_call.v2";

enum RoutineCallResult : int {
    // The routine returned; its output cells are written.
    ROUTINE_RETURNED = 0,
    // Nothing ran. The caller makes the call through Python instead.
    ROUTINE_DECLINED = 1,
    // The routine ran but stopped short of returning: it yielded at the
    // allowance, faulted or failed in the host. Python collects the outcome.
    ROUTINE_STOPPED = 2,
    // The routine stopped at a declared callback site. Its entry is parked in
    // the runner until resume() gives it the callback's output cells.
    ROUTINE_CALLBACK = 3,
};

inline constexpr std::uint32_t SPAN_READ = 1;
inline constexpr std::uint32_t SPAN_WRITE = 2;

struct RoutineSpan {
    std::uint64_t base;
    std::uint64_t size;
    std::uint32_t access;  // SPAN_READ and/or SPAN_WRITE
};

// Where a parked entry stopped for a callback. ``image`` is the published
// image holding the site, which machine code may have reached by calling
// another routine directly. ``slot`` is the machine stack pointer: the
// return-stack cell holding the CALL.L return address.
struct RoutineCallbackStop {
    std::uintptr_t image;
    std::uint64_t site;
    std::uint64_t slot;
    std::uint64_t argument_count;
    std::uint64_t arguments[8];
};

struct RoutineCall {
    void* runner;
    // ``image`` is the address of a published image object. ``instructions``
    // receives the machine instructions run, whatever the result. A callback
    // stop is written to ``callback``.
    int (*call)(void* runner, std::uintptr_t image,
                const std::uint64_t* arguments, std::size_t argument_count,
                const RoutineSpan* spans, std::size_t span_count,
                std::uint64_t frontier, std::uint64_t floor, std::uint64_t allowance,
                std::uint64_t* outputs, std::size_t output_count,
                RoutineCallbackStop* callback, std::uint64_t* instructions);
    // Resume the newest entry, parked for a callback at ``slot``, with that
    // callback's output cells. The results are those of call(); ``outputs``
    // receives the routine's own output cells when it returns.
    int (*resume)(void* runner, std::uint64_t slot,
                  const std::uint64_t* callback_outputs, std::size_t callback_output_count,
                  std::uint64_t allowance,
                  std::uint64_t* outputs, std::size_t output_count,
                  RoutineCallbackStop* callback, std::uint64_t* instructions);
};

}  // namespace megapad::hybrid
