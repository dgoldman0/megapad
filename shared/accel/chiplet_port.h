#pragma once

// MegaPad's side of a chiplet link, as the emulator and the simulator model
// it: the port bridge with its registers and lent ranges, and the register
// window that holds the ports (docs/chiplet-link.md; the hardware is
// rtl/soc/mp64_chiplet_port.v and the window logic in rtl/soc/mp64_soc.v).
//
// A request completes inside the call that makes it, so the model has no
// request in flight: STATUS.BUSY reads 0, and the CANCELLED response is
// given only to a request made while the link is down. QOS and
// EXT_CORE_QOS hold what software wrote and shape nothing, since the model
// has no cycle-level arbitration at external memory.
//
// The model has no Python dependency. Each backend supplies its external
// memory and its mailbox through the two interfaces below.

#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

#include "chiplet_module.h"
#include "mailbox_pending.h"

namespace megapad::chiplet {

// MegaPad's external memory, the only memory a chiplet can be lent.
class ExternalMemory {
public:
    virtual ~ExternalMemory() = default;
    // The half-open address range it occupies; base == limit when the
    // machine has none.
    virtual std::uint64_t base() const noexcept = 0;
    virtual std::uint64_t limit() const noexcept = 0;
    // Copy bytes that lie wholly inside the range. False reports a memory
    // error; no byte is then known to have moved.
    virtual bool read(std::uint64_t addr, std::uint8_t* out,
                      std::size_t bytes) noexcept = 0;
    virtual bool write(std::uint64_t addr, const std::uint8_t* in,
                       std::size_t bytes) noexcept = 0;
};

// The mailbox as one port's bridge uses it.
class PortMailbox {
public:
    virtual ~PortMailbox() = default;
    // The lowest ID whose doorbell waits for the port, or
    // MailboxPending::no_source.
    virtual int doorbell(int port) noexcept = 0;
    virtual bool take(int port, int source_id) noexcept = 0;
    // Ring target_id as the port.
    virtual bool send(int port, int target_id) noexcept = 0;
};

// A PortMailbox over a bare table, for a machine whose cores do not run
// while a port is served.
class TableMailbox final : public PortMailbox {
public:
    explicit TableMailbox(MailboxPending* table) noexcept : table_(table) {}
    int doorbell(int port) noexcept override {
        return table_->lowest_source(table_->port_id(port));
    }
    bool take(int port, int source_id) noexcept override {
        return table_->acknowledge(table_->port_id(port), source_id);
    }
    bool send(int port, int target_id) noexcept override {
        return table_->send(table_->port_id(port), target_id);
    }

private:
    MailboxPending* table_;
};

class ChipletPort {
public:
    // Register indices: the register's offset in the port block, over 8.
    enum Register : unsigned {
        CTRL = 0x0,
        STATUS = 0x1,
        FAULT_ADDR = 0x2,
        FAULT_INFO = 0x3,
        RANGE_SEL = 0x4,
        RANGE_BASE = 0x5,
        RANGE_LIMIT = 0x6,
        RANGE_RIGHTS = 0x7,
        QOS = 0x8,
        ID_INDEX = 0x9,
        ID_DATA = 0xA,
        BOOT_ADDR = 0xB,
        BOOT_LEN = 0xC,
        BOOT_ARG = 0xD,
    };

    // CTRL bits.
    static constexpr std::uint64_t CTRL_ENABLE = 1u << 0;
    static constexpr std::uint64_t CTRL_RESET_HOLD = 1u << 1;
    static constexpr std::uint64_t CTRL_STOP_REQ = 1u << 2;
    static constexpr std::uint64_t CTRL_DOORBELL_EN = 1u << 3;
    static constexpr std::uint64_t CTRL_ATTN_EN = 1u << 4;

    // STATUS bits.
    static constexpr std::uint64_t STATUS_PRESENT = 1u << 0;
    static constexpr std::uint64_t STATUS_STOPPED = 1u << 1;
    static constexpr std::uint64_t STATUS_READY = 1u << 2;
    static constexpr std::uint64_t STATUS_BUSY = 1u << 3;
    static constexpr std::uint64_t STATUS_FAULT = 1u << 4;
    static constexpr std::uint64_t STATUS_ID_DONE = 1u << 5;

    // Refusal reasons in FAULT_INFO.
    static constexpr unsigned WHY_CLOSED = 1;
    static constexpr unsigned WHY_MISALIGNED = 2;
    static constexpr unsigned WHY_NOT_EXTERNAL = 3;
    static constexpr unsigned WHY_NOT_LENT = 4;

    ChipletPort(int index, int id, int range_count, ExternalMemory* memory,
                PortMailbox* mailbox);
    ~ChipletPort();
    ChipletPort(const ChipletPort&) = delete;
    ChipletPort& operator=(const ChipletPort&) = delete;

    int index() const noexcept { return index_; }
    int id() const noexcept { return id_; }
    int range_count() const noexcept {
        return static_cast<int>(ranges_.size());
    }

    // --- MegaPad's side: the port block's registers ---
    std::uint64_t read_register(unsigned index) const noexcept;
    void write_register(unsigned index, std::uint64_t value) noexcept;

    // --- The chiplet behind the port ---
    // Attach a chiplet's model. The port owns the module from here on and
    // destroys it when another is attached or the port goes away. False
    // refuses a module of another interface version and leaves it alone.
    bool attach(const mp64_chiplet_module& module) noexcept;
    void detach() noexcept;
    bool attached() const noexcept { return attached_; }
    bool link_up() const noexcept { return attached_ && !reset_hold_; }

    // Offer the chiplet its waiting doorbells, lowest source first, and let
    // it work. Returns whether more work waits.
    bool step(std::uint32_t budget) noexcept;
    bool work_waits() const noexcept;

    // --- The chiplet's side of the link ---
    // The host services of chiplet_module.h arrive here.
    int request(std::uint64_t addr, unsigned size, unsigned len, bool wen,
                std::uint64_t* data, unsigned* done) noexcept;
    void ring(std::uint8_t dst) noexcept;
    void set_status(bool ready, bool stopped) noexcept;

    // --- The port's whole state, the attached module's included ---
    std::vector<std::uint8_t> save() const;
    bool restore(const std::vector<std::uint8_t>& state) noexcept;

private:
    struct Range {
        std::uint64_t base = 0;
        std::uint64_t limit = 0;
        std::uint8_t rights = 0;
    };

    unsigned refusal_reason(std::uint64_t addr, unsigned size, unsigned len,
                            bool wen) const noexcept;
    void record_refusal(std::uint64_t addr, unsigned size, unsigned len,
                        bool wen, unsigned reason) noexcept;
    void enter_reset() noexcept;
    void destroy_module() noexcept;

    const int index_;
    const int id_;
    ExternalMemory* const memory_;
    PortMailbox* const mailbox_;
    const unsigned range_sel_mask_;

    // CTRL
    bool enable_ = false;
    bool reset_hold_ = true;
    bool stop_req_ = false;
    bool doorbell_en_ = false;
    bool attn_en_ = false;
    std::uint8_t attn_core_ = 0;

    // Lent ranges
    unsigned range_sel_ = 0;
    std::uint64_t stage_base_ = 0;
    std::uint64_t stage_limit_ = 0;
    std::vector<Range> ranges_;

    std::uint8_t qos_weight_ = 1;
    std::uint16_t qos_limit_ = 0;
    std::uint64_t boot_addr_ = 0;
    std::uint64_t boot_len_ = 0;
    std::uint64_t boot_arg_ = 0;

    // Refusal record
    bool fault_ = false;
    std::uint64_t fault_addr_ = 0;
    std::uint8_t fault_size_ = 0;
    bool fault_wen_ = false;
    std::uint8_t fault_why_ = 0;
    std::uint8_t fault_len_ = 0;
    std::uint16_t refusals_ = 0;

    // Sideband
    bool id_done_ = false;
    std::uint8_t id_index_ = 0;
    std::uint64_t id_data_ = 0;
    bool ready_ = false;
    bool stopped_ = false;

    // The chiplet's model
    bool attached_ = false;
    mp64_chiplet_module module_{};
    mp64_chiplet_host host_{};
    bool more_work_ = false;
    bool doorbell_declined_ = false;
    bool active_ = false;       // the chiplet used the link in this step
};

// The register window at MMIO offset 0xE00: the shared registers and the
// selected port's block.
class ChipletWindow {
public:
    static constexpr unsigned SIZE = 0x100;
    static constexpr unsigned PORT_COUNT = 0x00;
    static constexpr unsigned PORT_SEL = 0x08;
    static constexpr unsigned EXT_CORE_QOS = 0x10;
    static constexpr unsigned PORT_BLOCK = 0x80;
    static constexpr std::uint32_t DEFAULT_BUDGET = 64;

    ChipletWindow(int port_count, int first_port_id, int range_count,
                  ExternalMemory* memory, PortMailbox* mailbox);

    int port_count() const noexcept {
        return static_cast<int>(ports_.size());
    }
    int first_port_id() const noexcept { return first_port_id_; }
    int range_count() const noexcept { return range_count_; }
    // PORT_COUNT's value, which System Info repeats: [7:0] ports,
    // [15:8] the first port's ID, [23:16] lent ranges per port.
    std::uint64_t port_count_word() const noexcept;

    ChipletPort* port(int index) noexcept {
        return index >= 0 && index < port_count()
            ? ports_[static_cast<std::size_t>(index)].get() : nullptr;
    }

    // One aligned 64-bit access; offset is 0 to 0xF8 in steps of 8. The
    // caller rejects every other shape as a bus fault.
    std::uint64_t read64(unsigned offset) const noexcept;
    void write64(unsigned offset, std::uint64_t value) noexcept;

    // The work a port's chiplet may do in one step.
    void set_budget(int port, std::uint32_t budget) noexcept;
    // Step every port once, in port order. Returns whether any chiplet has
    // more work waiting.
    bool step() noexcept;
    bool work_waits() const noexcept;

    std::vector<std::uint8_t> save() const;
    bool restore(const std::vector<std::uint8_t>& state) noexcept;

private:
    const int first_port_id_;
    const int range_count_;
    std::uint8_t port_sel_ = 0;
    std::uint8_t ext_core_qos_ = 1;
    std::vector<std::unique_ptr<ChipletPort>> ports_;
    std::vector<std::uint32_t> budgets_;
};

// Load a chiplet's model from a module library built outside MegaPad: the
// library exports mp64_chiplet_create (chiplet_module.h). Returns whether
// `out` now holds a module; on failure `error` says why. The library stays
// loaded for the life of the process.
bool load_module(const char* path, const char* config,
                 mp64_chiplet_module* out, std::string* error);

// The test chiplet built into this extension (chiplet_test_module.cpp).
int create_test_chiplet(const char* config, mp64_chiplet_module* out);

}  // namespace megapad::chiplet
