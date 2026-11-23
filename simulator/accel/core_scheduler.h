#pragma once

// The hosted simulator's scheduler of full cores
// (docs/multicore-simulator-plan.md, sections 4.3 and 4.4).
//
// A round visits the awake cores in ascending core ID, and each core's turn
// runs for at most the quantum's semantic steps. A turn ends early when the
// core sleeps in IDL, finds a spinlock busy, or finishes its work: a
// worker's job, or core 0's host dispatch. A core woken during a round takes
// its turn in that round when its ID comes after the core whose turn woke
// it, and in the next round otherwise. The scheduler decides which core runs
// and when a sleeping core wakes; the runtime's glue runs each turn and
// reports what ended it.
//
// A core is awake, asleep in IDL (with a deadline in MS@ milliseconds or
// none), or idle: a worker with no job, or core 0 with no host dispatch.
// A sleeping or idle core wakes when its IPI line is raised, core 0 also
// when terminal input is waiting, and a sleeping core at its deadline.
//
// Time follows the round: the glue starts every turn at the round's start
// and moves the clock on by the round's longest turn when the round ends,
// which finish_round() returns.
//
// The scheduler has no Python dependency.

#include <cstdint>
#include <limits>
#include <stdexcept>
#include <vector>

namespace megapad::scheduler {

enum class State : std::uint8_t { AWAKE, ASLEEP, IDLE };

// What ended a turn.
enum class TurnEnd : std::uint8_t {
    QUANTUM,  // the turn used its quantum; the core stays awake
    SPIN,     // the core found a spinlock busy; it stays awake
    SLEEP,    // the core sleeps in IDL
    DONE,     // the core finished its work and goes idle
};

class CoreScheduler {
public:
    static constexpr std::uint64_t NO_DEADLINE =
        std::numeric_limits<std::uint64_t>::max();
    static constexpr int MAX_CORES = 64;

    CoreScheduler(int cores, std::uint64_t quantum)
        : quantum_(quantum),
          state_(check_cores(cores), State::IDLE),
          deadline_(static_cast<std::size_t>(cores), NO_DEADLINE),
          steps_(static_cast<std::size_t>(cores), 0),
          turns_(static_cast<std::size_t>(cores), 0) {
        if (quantum == 0)
            throw std::invalid_argument("the turn quantum must be positive");
    }

    int cores() const noexcept { return static_cast<int>(state_.size()); }
    std::uint64_t quantum() const noexcept { return quantum_; }

    State state(int core) const { return state_.at(index(core)); }
    std::uint64_t deadline(int core) const { return deadline_.at(index(core)); }
    std::uint64_t steps(int core) const { return steps_.at(index(core)); }
    std::uint64_t turns(int core) const { return turns_.at(index(core)); }

    bool awake(int core) const { return state(core) == State::AWAKE; }

    bool any_awake() const noexcept {
        for (State value : state_)
            if (value == State::AWAKE)
                return true;
        return false;
    }

    // ---- Rounds and turns ----

    // The core whose turn is running, or -1 between turns.
    int current() const noexcept { return current_; }

    // Begin the next turn of this round: the lowest awake core after the
    // last one served. -1 when the round has no further turn.
    int begin_next_turn() {
        if (current_ >= 0)
            throw std::logic_error("a turn is already running");
        for (int core = next_; core < cores(); ++core) {
            if (state_[index(core)] == State::AWAKE) {
                current_ = core;
                next_ = core + 1;
                used_ = 0;
                round_started_ = true;
                return core;
            }
        }
        next_ = cores();
        return -1;
    }

    // Steps the running turn may still take.
    std::uint64_t turn_left() const {
        require_turn();
        return used_ >= quantum_ ? 0 : quantum_ - used_;
    }

    std::uint64_t turn_used() const {
        require_turn();
        return used_;
    }

    // The running turn took `count` more steps.
    void account(std::uint64_t count) {
        require_turn();
        used_ += count;
        steps_[index(current_)] += count;
    }

    // The running turn ended for `reason`. A sleep may name its deadline.
    void end_turn(TurnEnd reason, std::uint64_t deadline = NO_DEADLINE) {
        require_turn();
        const std::size_t core = index(current_);
        turns_[core] += 1;
        if (used_ > longest_)
            longest_ = used_;
        switch (reason) {
        case TurnEnd::QUANTUM:
        case TurnEnd::SPIN:
            break;
        case TurnEnd::SLEEP:
            state_[core] = State::ASLEEP;
            deadline_[core] = deadline;
            break;
        case TurnEnd::DONE:
            state_[core] = State::IDLE;
            deadline_[core] = NO_DEADLINE;
            break;
        }
        current_ = -1;
    }

    // Whether this round has served a turn and not yet been finished.
    bool round_started() const noexcept { return round_started_; }

    // The round's length so far: its longest turn.
    std::uint64_t round_length() const noexcept { return longest_; }

    // Finish the round and return its length, the longest turn's steps.
    std::uint64_t finish_round() {
        if (current_ >= 0)
            throw std::logic_error("a turn is still running");
        const std::uint64_t length = longest_;
        next_ = 0;
        longest_ = 0;
        round_started_ = false;
        return length;
    }

    // ---- Waking ----

    // The core has work: a job, a host dispatch, or a signal to take.
    void wake(int core) {
        const std::size_t at = index(core);
        state_[at] = State::AWAKE;
        deadline_[at] = NO_DEADLINE;
    }

    // The core has no work left between its turns: it goes idle, as a turn
    // that finishes its work leaves it.
    void rest(int core) {
        const std::size_t at = index(core);
        if (current_ == core)
            throw std::logic_error("the core's turn is running");
        state_[at] = State::IDLE;
        deadline_[at] = NO_DEADLINE;
    }

    // The sleeping or idle cores whose wake rule holds, as a mask: a raised
    // IPI line (bit n of `ipi_lines` for core n), terminal input for core 0,
    // or a deadline at or before `now_ms` for a sleeping core.
    std::uint64_t wake_mask(std::uint64_t ipi_lines, bool terminal_input,
                            std::uint64_t now_ms) const noexcept {
        std::uint64_t mask = 0;
        for (int core = 0; core < cores(); ++core) {
            const std::size_t at = static_cast<std::size_t>(core);
            const State value = state_[at];
            if (value == State::AWAKE)
                continue;
            const bool line = (ipi_lines >> core) & 1;
            const bool input = core == 0 && terminal_input;
            const bool due = value == State::ASLEEP &&
                deadline_[at] != NO_DEADLINE && deadline_[at] <= now_ms;
            if (line || input || due)
                mask |= std::uint64_t{1} << core;
        }
        return mask;
    }

    // Wake every core whose rule holds; returns them as a mask.
    std::uint64_t apply_wakes(std::uint64_t ipi_lines, bool terminal_input,
                              std::uint64_t now_ms) {
        const std::uint64_t woken = wake_mask(ipi_lines, terminal_input, now_ms);
        for (int core = 0; core < cores(); ++core)
            if ((woken >> core) & 1)
                wake(core);
        return woken;
    }

    // The earliest deadline of a sleeping core, or NO_DEADLINE.
    std::uint64_t earliest_deadline() const noexcept {
        std::uint64_t earliest = NO_DEADLINE;
        for (std::size_t at = 0; at < state_.size(); ++at)
            if (state_[at] == State::ASLEEP && deadline_[at] < earliest)
                earliest = deadline_[at];
        return earliest;
    }

private:
    static int check_cores(int cores) {
        if (cores < 1 || cores > MAX_CORES)
            throw std::invalid_argument("a scheduler serves 1 to 64 cores");
        return cores;
    }

    std::size_t index(int core) const {
        if (core < 0 || core >= cores())
            throw std::out_of_range("no such core");
        return static_cast<std::size_t>(core);
    }

    void require_turn() const {
        if (current_ < 0)
            throw std::logic_error("no turn is running");
    }

    std::uint64_t quantum_;
    std::vector<State> state_;
    std::vector<std::uint64_t> deadline_;
    std::vector<std::uint64_t> steps_;
    std::vector<std::uint64_t> turns_;
    int current_ = -1;
    int next_ = 0;
    std::uint64_t used_ = 0;
    std::uint64_t longest_ = 0;
    bool round_started_ = false;
};

}  // namespace megapad::scheduler
