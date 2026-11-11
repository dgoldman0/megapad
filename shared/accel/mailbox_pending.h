#pragma once

// Pending signals among the mailbox's participants.
//
// The mailbox's participants are the cores and the chiplet ports. Cores hold
// IDs 0 to core_count - 1, and port p holds first_port_id + p, where the
// first port's ID is at or above the core count (rtl/periph/mp64_mailbox.v,
// docs/chiplet-link.md section 6). Each participant has one pending bit for
// every other participant that has signalled it. A core's IPI line follows
// its pending bits, and its external-flag line follows the bits that ports
// set. A port is offered its lowest pending source.
//
// The table holds state alone. A caller that shares it between threads holds
// its own lock and its own mirrors of the lines, as the emulator's interrupt
// router does. It has no Python dependency.

#include <cstddef>
#include <cstdint>
#include <vector>

namespace megapad::chiplet {

class MailboxPending {
public:
    static constexpr int no_source = -1;

    MailboxPending() = default;
    MailboxPending(int core_count, int port_count, int first_port_id) {
        configure(core_count, port_count, first_port_id);
    }

    // Lay the table out for these participants and clear it. first_port_id
    // below the core count places the ports directly after the cores.
    void configure(int core_count, int port_count, int first_port_id = -1) {
        cores_ = core_count < 0 ? 0 : core_count;
        ports_ = port_count < 0 ? 0 : port_count;
        first_port_id_ = first_port_id < cores_ ? cores_ : first_port_id;
        words_ = (static_cast<std::size_t>(cores_ + ports_) + 63) / 64;
        bits_.assign(static_cast<std::size_t>(cores_ + ports_) * words_, 0);
    }

    void clear() noexcept {
        for (std::uint64_t& word : bits_)
            word = 0;
    }

    int core_count() const noexcept { return cores_; }
    int port_count() const noexcept { return ports_; }
    int first_port_id() const noexcept { return first_port_id_; }
    int port_id(int port) const noexcept { return first_port_id_ + port; }

    bool is_core(int id) const noexcept { return id >= 0 && id < cores_; }
    bool is_port(int id) const noexcept {
        return id >= first_port_id_ && id - first_port_id_ < ports_;
    }
    bool is_participant(int id) const noexcept {
        return is_core(id) || is_port(id);
    }

    // source_id signals target_id. A signal that names no participant, or
    // that a participant sends to itself, is ignored. Returns whether the
    // signal was accepted; a second signal from the same source joins the
    // one already pending.
    bool send(int source_id, int target_id) noexcept {
        if (!is_participant(source_id) || !is_participant(target_id) ||
            source_id == target_id)
            return false;
        const std::size_t source = index_of(source_id);
        row(target_id)[source / 64] |= std::uint64_t{1} << (source % 64);
        return true;
    }

    // target_id takes or acknowledges the signal from source_id. Returns
    // whether one was pending.
    bool acknowledge(int target_id, int source_id) noexcept {
        if (!is_participant(target_id) || !is_participant(source_id))
            return false;
        const std::size_t source = index_of(source_id);
        std::uint64_t& word = row(target_id)[source / 64];
        const std::uint64_t bit = std::uint64_t{1} << (source % 64);
        const bool was_pending = (word & bit) != 0;
        word &= ~bit;
        return was_pending;
    }

    bool is_pending(int target_id, int source_id) const noexcept {
        if (!is_participant(target_id) || !is_participant(source_id))
            return false;
        const std::size_t source = index_of(source_id);
        return (row(target_id)[source / 64] >> (source % 64)) & 1;
    }

    // Whether any participant has signalled target_id: a core's IPI line, or
    // a port's doorbell line.
    bool any_pending(int target_id) const noexcept {
        if (!is_participant(target_id))
            return false;
        const std::uint64_t* words = row(target_id);
        for (std::size_t word = 0; word < words_; ++word)
            if (words[word] != 0)
                return true;
        return false;
    }

    // Whether a port has signalled target_id: a core's external-flag line.
    bool port_pending(int target_id) const noexcept {
        if (!is_participant(target_id))
            return false;
        for (int port = 0; port < ports_; ++port)
            if (is_pending(target_id, port_id(port)))
                return true;
        return false;
    }

    // The lowest ID that has signalled target_id, or no_source.
    int lowest_source(int target_id) const noexcept {
        if (!is_participant(target_id))
            return no_source;
        const std::uint64_t* words = row(target_id);
        for (std::size_t word = 0; word < words_; ++word) {
            if (words[word] == 0)
                continue;
            const std::size_t source =
                word * 64 +
                static_cast<std::size_t>(__builtin_ctzll(words[word]));
            return id_of(source);
        }
        return no_source;
    }

    // target_id's pending bits by source ID, for the IDs below 64.
    std::uint64_t pending_mask(int target_id) const noexcept {
        if (!is_participant(target_id))
            return 0;
        std::uint64_t mask = 0;
        const std::size_t participants =
            static_cast<std::size_t>(cores_ + ports_);
        const std::uint64_t* words = row(target_id);
        for (std::size_t source = 0; source < participants; ++source) {
            if (((words[source / 64] >> (source % 64)) & 1) == 0)
                continue;
            const int id = id_of(source);
            if (id < 64)
                mask |= std::uint64_t{1} << id;
        }
        return mask;
    }

    // Drop every signal that waits for target_id.
    void clear_target(int target_id) noexcept {
        if (!is_participant(target_id))
            return;
        std::uint64_t* words = row(target_id);
        for (std::size_t word = 0; word < words_; ++word)
            words[word] = 0;
    }

    // The whole table, for a snapshot: one row of words per participant, in
    // participant order.
    const std::vector<std::uint64_t>& words() const noexcept { return bits_; }
    bool restore_words(const std::vector<std::uint64_t>& words) {
        if (words.size() != bits_.size())
            return false;
        bits_ = words;
        return true;
    }

private:
    std::size_t index_of(int id) const noexcept {
        return id < cores_
            ? static_cast<std::size_t>(id)
            : static_cast<std::size_t>(cores_ + (id - first_port_id_));
    }
    int id_of(std::size_t index) const noexcept {
        return index < static_cast<std::size_t>(cores_)
            ? static_cast<int>(index)
            : first_port_id_ + static_cast<int>(index) - cores_;
    }
    std::uint64_t* row(int id) noexcept {
        return bits_.data() + index_of(id) * words_;
    }
    const std::uint64_t* row(int id) const noexcept {
        return bits_.data() + index_of(id) * words_;
    }

    int cores_ = 0;
    int ports_ = 0;
    int first_port_id_ = 0;
    std::size_t words_ = 0;
    std::vector<std::uint64_t> bits_;
};

}  // namespace megapad::chiplet
