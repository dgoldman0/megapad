#pragma once

// MegaPad's mailbox as an MMIO device, one model for the emulator and the
// simulator.
//
// Every core signals any participant through the mailbox, cores and chiplet
// ports alike (rtl/periph/mp64_mailbox.v, docs/chiplet-link.md section 4).
// Its 16 bytes at MMIO 0x500 read and write as the requesting core sees
// them:
//
//   +0 to +7  DATA    the requester's data word, little-endian
//   +8        SEND    write an ID: the requester signals that participant;
//                     a core that is signalled also takes a copy of the
//                     sender's data word
//   +9        STATUS  read: the requester's pending bits for IDs 0 to 7
//   +A        ACK     write an ID: the requester takes that participant's
//                     signal
//
// Every other byte reads 0 and ignores writes. An access moves 1, 2, 4 or 8
// bytes, naturally aligned, inside the window. A requester that names no
// core reads 0 and changes nothing.
//
// The pending bits are a MailboxPending table that the mailbox owns or
// borrows: a backend whose chiplet ports ring cores through a table of
// their own lends the mailbox that table. A hook runs before each read of
// STATUS, where a backend that steps its chiplets on demand lets them catch
// up, so a core that polls for a port's signal makes progress.
//
// The model has no Python dependency.

#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <vector>

#include "mailbox_pending.h"
#include "mmio_device.h"

namespace megapad::mailbox {

class Mailbox : public mmio::Device {
public:
    static constexpr mmio::Offset OFFSET = 0x500;
    static constexpr mmio::Offset SIZE = 0x10;
    static constexpr mmio::Offset LIMIT = OFFSET + SIZE;
    static constexpr mmio::Offset SEND = OFFSET + 0x08;
    static constexpr mmio::Offset STATUS = OFFSET + 0x09;
    static constexpr mmio::Offset ACK = OFFSET + 0x0A;

    // A mailbox with a table of its own, for `core_count` cores and no
    // ports.
    explicit Mailbox(int core_count)
        : owned_(std::make_unique<chiplet::MailboxPending>(core_count, 0,
                                                           core_count)),
          table_(owned_.get()),
          data_(static_cast<std::size_t>(table_->core_count()), 0) {}

    // A mailbox over a table that outlives it.
    explicit Mailbox(chiplet::MailboxPending* table)
        : table_(table),
          data_(static_cast<std::size_t>(table->core_count()), 0) {}

    chiplet::MailboxPending& table() noexcept { return *table_; }
    const chiplet::MailboxPending& table() const noexcept { return *table_; }
    int core_count() const noexcept { return table_->core_count(); }

    void set_before_status(std::function<void()> hook) {
        before_status_ = std::move(hook);
    }

    // A core's data word; zero for an ID that names no core.
    std::uint64_t data(int core) const noexcept {
        return table_->is_core(core) ? data_[static_cast<std::size_t>(core)]
                                     : 0;
    }
    void set_data(int core, std::uint64_t value) noexcept {
        if (table_->is_core(core))
            data_[static_cast<std::size_t>(core)] = value;
    }

    // `source` signals `target` as a write of the ID to SEND does.
    bool send(int source, int target) noexcept {
        if (!table_->is_core(source) || source == target)
            return false;
        if (!table_->send(source, target))
            return false;
        if (table_->is_core(target))
            data_[static_cast<std::size_t>(target)] =
                data_[static_cast<std::size_t>(source)];
        return true;
    }

    // Reset: no signal pending and every data word zero.
    void reset() noexcept {
        table_->clear();
        for (std::uint64_t& word : data_)
            word = 0;
    }

    mmio::Verdict preflight(mmio::Offset offset, unsigned width, bool write,
                            mmio::Requester requester) override {
        static_cast<void>(write);
        static_cast<void>(requester);
        if (!mmio::scalar_width(width) || offset % width != 0 ||
            !mmio::inside(offset, width, OFFSET, LIMIT))
            return mmio::Verdict::refuse(
                "access is outside the mailbox or not naturally aligned");
        return mmio::Verdict::admit();
    }

    std::uint8_t read8(mmio::Offset offset,
                       mmio::Requester requester) override {
        require_byte(offset);
        if (!table_->is_core(requester))
            return 0;
        const mmio::Offset reg = offset - OFFSET;
        if (reg < 8)
            return static_cast<std::uint8_t>(
                data_[static_cast<std::size_t>(requester)] >> (8 * reg));
        if (offset == STATUS) {
            if (before_status_)
                before_status_();
            return static_cast<std::uint8_t>(
                table_->pending_mask(requester) & 0xFF);
        }
        return 0;
    }

    void write8(mmio::Offset offset, std::uint8_t value,
                mmio::Requester requester) override {
        require_byte(offset);
        if (!table_->is_core(requester))
            return;
        const mmio::Offset reg = offset - OFFSET;
        if (reg < 8) {
            std::uint64_t& word = data_[static_cast<std::size_t>(requester)];
            const unsigned shift = static_cast<unsigned>(8 * reg);
            word = (word & ~(std::uint64_t{0xFF} << shift)) |
                   (std::uint64_t{value} << shift);
        } else if (offset == SEND) {
            send(requester, value);
        } else if (offset == ACK) {
            table_->acknowledge(requester, value);
        }
    }

    unsigned lines(int core) const noexcept override {
        unsigned raised = 0;
        if (table_->any_pending(core))
            raised |= mmio::LINE_IPI;
        if (table_->port_pending(core))
            raised |= mmio::LINE_EXTERNAL_FLAG;
        return raised;
    }

private:
    static void require_byte(mmio::Offset offset) {
        if (offset < OFFSET || offset >= LIMIT)
            throw mmio::Refused("byte access is outside the mailbox");
    }

    std::unique_ptr<chiplet::MailboxPending> owned_;
    chiplet::MailboxPending* table_;
    std::vector<std::uint64_t> data_;
    std::function<void()> before_status_;
};

}  // namespace megapad::mailbox
