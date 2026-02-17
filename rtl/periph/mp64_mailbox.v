// ============================================================================
// mp64_mailbox.v — Inter-Core Mailbox & Hardware Spinlocks
// ============================================================================
//
// Provides two multi-core synchronisation primitives:
//
// 1. MAILBOX  — 4 × 64-bit message slots + IPI (inter-processor interrupt)
//    Each core has a 64-bit data register.  Core A writes data, then writes
//    target core B's ID to MBOX_SEND → sets pending bit on core B → fires
//    core B's IPI line.  Core B reads data + ACKs to clear pending.
//
// 2. SPINLOCKS — 8 hardware test-and-set locks
//    Read SLOCK_ACQUIRE+N*4: returns 0 if lock acquired (was free),
//    returns 1 if lock is busy (held by another core).  Atomic in hardware.
//    Write SLOCK_RELEASE+N*4: releases lock N.
//    Each lock records the owning core ID for debugging.
//
// MMIO interface: 8-bit register reads/writes at MBOX_BASE and SPINLOCK_BASE.
// The core_id of the requesting core is provided by the SoC (from the bus
// arbiter's current grant).
//

`include "mp64_pkg.vh"

module mp64_mailbox #(
    parameter N_CORES = MP64_NUM_CORES_DEFAULT,     // number of cores to support
    parameter ID_BITS = MP64_CORE_ID_BITS   // bits for core ID
) (
    input  wire        clk,
    input  wire        rst_n,

    // === MMIO interface (from bus arbiter) ===
    input  wire        req,
    input  wire [11:0] addr,          // full 12-bit MMIO offset
    input  wire [7:0]  wdata,
    input  wire        wen,
    output reg  [7:0]  rdata,
    output wire        ack,

    // === Which core is making this request (from arbiter grant) ===
    input  wire [ID_BITS-1:0] requester_id,

    // === IPI outputs — active-high, one per core ===
    output wire [N_CORES-1:0]    ipi_out,

    // === Per-core CSR-side IPI interface ===
    // Allows each CPU to access mailbox via CSR instructions (CSR_MBOX,
    // CSR_IPIACK) without going through the bus arbiter.
    input  wire [N_CORES-1:0]       csr_ipi_wen,     // per-core CSR write enable
    input  wire [N_CORES*8-1:0]     csr_ipi_addr,    // CSR address (0x22 or 0x23)
    input  wire [N_CORES*64-1:0]    csr_ipi_wdata,   // CSR write data
    output wire [N_CORES*64-1:0]    csr_ipi_rdata    // per-core: pending mask
);

    assign ack = 1'b1;  // single-cycle MMIO

    // ========================================================================
    // Mailbox registers
    // ========================================================================
    // Per-core: 64-bit data, 4-bit pending mask (which cores sent to us)
    reg [63:0] mbox_data    [0:N_CORES-1];   // outgoing data per core
    reg [N_CORES-1:0] mbox_pending [0:N_CORES-1]; // pending IPIs per target

    // IPI output = any pending bit set for that core
    genvar ci;
    generate
        for (ci = 0; ci < N_CORES; ci = ci + 1) begin : ipi_gen
            assign ipi_out[ci] = |mbox_pending[ci];
        end
    endgenerate

    // ========================================================================
    // CSR-side IPI interface (per-core, no arbiter needed)
    // ========================================================================
    // CSR_MBOX (0x22) read  → returns pending mask for that core
    // CSR_MBOX (0x22) write → send IPI (wdata[ID_BITS-1:0] = target core)
    // CSR_IPIACK (0x23) write → ack IPI (wdata[ID_BITS-1:0] = source core)
    generate
        for (ci = 0; ci < N_CORES; ci = ci + 1) begin : csr_ipi_rdata_gen
            assign csr_ipi_rdata[ci*64 +: 64] = {{(64-N_CORES){1'b0}}, mbox_pending[ci]};
        end
    endgenerate

    // Address decode within MBOX region
    wire is_mbox  = (addr[11:8] == 4'h5);  // MBOX_BASE = 0x500
    wire is_slock = (addr[11:8] == 4'h6);  // SPINLOCK_BASE = 0x600

    // Mailbox sub-register select (per requester)
    wire [3:0] mbox_reg = addr[3:0];

    // ========================================================================
    // Hardware spinlocks
    // ========================================================================
    reg [N_CORES-1:0] slock_held  [0:MP64_NUM_SPINLOCKS-1]; // bit mask: who holds
    reg                slock_locked[0:MP64_NUM_SPINLOCKS-1]; // is lock held?
    reg [ID_BITS-1:0]  slock_owner[0:MP64_NUM_SPINLOCKS-1]; // owning core

    // Spinlock index from address: lock N at offset N*4
    wire [2:0] slock_idx = addr[4:2];
    wire [3:0] slock_reg = addr[1:0];

    // ========================================================================
    // Read mux
    // ========================================================================
    integer ri;
    always @(*) begin
        rdata = 8'd0;

        if (is_mbox) begin
            case (mbox_reg)
                MBOX_DATA_LO: rdata = mbox_data[requester_id][7:0];
                4'h1:         rdata = mbox_data[requester_id][15:8];
                4'h2:         rdata = mbox_data[requester_id][23:16];
                4'h3:         rdata = mbox_data[requester_id][31:24];
                MBOX_DATA_HI: rdata = mbox_data[requester_id][39:32];
                4'h5:         rdata = mbox_data[requester_id][47:40];
                4'h6:         rdata = mbox_data[requester_id][55:48];
                4'h7:         rdata = mbox_data[requester_id][63:56];
                MBOX_STATUS:  rdata = mbox_pending[requester_id][7:0];
                default:      rdata = 8'd0;
            endcase
        end else if (is_slock) begin
            // Read SLOCK_ACQUIRE: returns 0=acquired, 1=busy
            // (actual acquisition happens in sequential block below)
            if (slock_reg == SLOCK_ACQUIRE[1:0]) begin
                rdata = {7'd0, slock_locked[slock_idx] &&
                         (slock_owner[slock_idx] != requester_id)};
            end
        end
    end

    // ========================================================================
    // Write logic (sequential)
    // ========================================================================
    integer wi;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            for (wi = 0; wi < N_CORES; wi = wi + 1) begin
                mbox_data[wi]    <= 64'd0;
                mbox_pending[wi] <= {N_CORES{1'b0}};
            end
            for (wi = 0; wi < MP64_NUM_SPINLOCKS; wi = wi + 1) begin
                slock_locked[wi] <= 1'b0;
                slock_held[wi]   <= {N_CORES{1'b0}};
                slock_owner[wi]  <= {ID_BITS{1'b0}};
            end
        end else begin

            // --- Mailbox writes ---
            if (req && wen && is_mbox) begin
                case (mbox_reg)
                    // Write outgoing data (stored per requester)
                    MBOX_DATA_LO: mbox_data[requester_id][7:0]   <= wdata;
                    4'h1:         mbox_data[requester_id][15:8]   <= wdata;
                    4'h2:         mbox_data[requester_id][23:16]  <= wdata;
                    4'h3:         mbox_data[requester_id][31:24]  <= wdata;
                    MBOX_DATA_HI: mbox_data[requester_id][39:32]  <= wdata;
                    4'h5:         mbox_data[requester_id][47:40]  <= wdata;
                    4'h6:         mbox_data[requester_id][55:48]  <= wdata;
                    4'h7:         mbox_data[requester_id][63:56]  <= wdata;

                    // SEND: write target core ID → set pending bit on target
                    MBOX_SEND: begin
                        if (wdata[ID_BITS-1:0] < N_CORES) begin
                            mbox_pending[wdata[ID_BITS-1:0]][requester_id] <= 1'b1;
                        end
                    end

                    // ACK: write source core ID → clear pending bit from that source
                    MBOX_ACK: begin
                        if (wdata[ID_BITS-1:0] < N_CORES) begin
                            mbox_pending[requester_id][wdata[ID_BITS-1:0]] <= 1'b0;
                        end
                    end
                endcase
            end

            // --- Spinlock acquire (on read of SLOCK_ACQUIRE) ---
            if (req && !wen && is_slock && (slock_reg == SLOCK_ACQUIRE[1:0])) begin
                if (!slock_locked[slock_idx]) begin
                    // Lock was free — acquire it
                    slock_locked[slock_idx] <= 1'b1;
                    slock_owner[slock_idx]  <= requester_id;
                    slock_held[slock_idx][requester_id] <= 1'b1;
                end
                // If already held by us, that's a re-entrant acquire (OK)
                // If held by another core, read returns 1 (busy), no state change
            end

            // --- Spinlock release (on write to SLOCK_RELEASE) ---
            if (req && wen && is_slock && (slock_reg == SLOCK_RELEASE[1:0])) begin
                if (slock_locked[slock_idx] &&
                    slock_owner[slock_idx] == requester_id) begin
                    slock_locked[slock_idx] <= 1'b0;
                    slock_held[slock_idx][requester_id] <= 1'b0;
                end
            end

            // --- CSR-side IPI send/ack (per-core, bypasses bus arbiter) ---
            for (wi = 0; wi < N_CORES; wi = wi + 1) begin
                if (csr_ipi_wen[wi]) begin
                    if (csr_ipi_addr[wi*8 +: 8] == CSR_MBOX) begin
                        // CSR_MBOX write: send IPI to target core
                        if (csr_ipi_wdata[wi*64 +: 64] < N_CORES) begin
                            mbox_pending[csr_ipi_wdata[wi*64 +: ID_BITS]][wi] <= 1'b1;
                        end
                    end else if (csr_ipi_addr[wi*8 +: 8] == CSR_IPIACK) begin
                        // CSR_IPIACK write: ack IPI from source core
                        if (csr_ipi_wdata[wi*64 +: 64] < N_CORES) begin
                            mbox_pending[wi][csr_ipi_wdata[wi*64 +: ID_BITS]] <= 1'b0;
                        end
                    end
                end
            end
        end
    end

endmodule
