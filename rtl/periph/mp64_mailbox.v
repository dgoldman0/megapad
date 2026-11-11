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
// 2. SPINLOCKS — 16 hardware test-and-set locks
//    Read SLOCK_ACQUIRE+N*4: returns 0 if lock acquired (was free),
//    returns 1 if lock is busy (held by another core).  Atomic in hardware.
//    Write SLOCK_RELEASE+N*4: releases lock N.
//    Each lock records the owning core ID for debugging.
//
// MMIO interface: 8-bit register reads/writes at MBOX_BASE and SPINLOCK_BASE.
// The core_id of the requesting core is provided by the SoC (from the bus
// arbiter's current grant).
//
// The participants are the cores, full and small, by their global IDs, and
// the chiplet ports.  A small core has an IPI line and an external-flag line
// as a full core has, and reaches the mailbox through its registers, with
// its ID given by the arbiter; the CSR path serves the full cores.
//
// Port p carries the ID PORT_ID_BASE + p.  A core rings a port by sending to
// that ID, and a port rings a core, or another port, through its send inputs.
// For each port the mailbox offers its lowest pending source as an ID, and
// the port's bridge clears that bit once the chiplet has taken the doorbell.
// A pending bit from a port also raises its target core's external-flag
// line, so the core can test for a chiplet's signal with a branch.  With
// N_PORTS = 0 the mailbox serves the cores alone.
//
// A core's pending mask is indexed by ID: bit n is set while the participant
// with ID n has signalled the core.  MBOX_STATUS shows IDs 0 to 7, and the
// CSR path returns all 64 bits, where a port's bit is at its ID.  A
// participant's signal to itself is ignored, as the emulator ignores it.
//

`include "mp64_pkg.vh"

module mp64_mailbox #(
    parameter N_CORES = MP64_NUM_CORES_DEFAULT, // full cores: the CSR path
    parameter N_GLOBAL_CORES = N_CORES,       // full + small cores: mailbox
                                              // participants, spinlock owners
    parameter ID_BITS = MP64_CORE_ID_BITS,     // bits for global core ID
    parameter N_PORTS = 0,                     // chiplet ports
    parameter [ID_BITS-1:0] PORT_ID_BASE = 16, // ID of port 0
    // Port vectors keep one element when no port exists; it is unused then.
    parameter NP_W = (N_PORTS > 0) ? N_PORTS : 1
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
    input  wire               requester_valid,
    input  wire [ID_BITS-1:0] requester_id,

    // === IPI outputs — active-high, one per core, full then small ===
    output wire [N_GLOBAL_CORES-1:0] ipi_out,

    // === Per-core CSR-side IPI interface ===
    // Allows each CPU to access mailbox via CSR instructions (CSR_MBOX,
    // CSR_IPIACK) without going through the bus arbiter.
    input  wire [N_CORES-1:0]       csr_ipi_wen,     // per-core CSR write enable
    input  wire [N_CORES*8-1:0]     csr_ipi_addr,    // CSR address (0x22 or 0x23)
    input  wire [N_CORES*64-1:0]    csr_ipi_wdata,   // CSR write data
    output wire [N_CORES*64-1:0]    csr_ipi_rdata,   // per-core: pending mask

    // === Chiplet ports as participants ===
    input  wire [NP_W-1:0]          port_send_valid, // ring port_send_dst
    input  wire [NP_W*ID_BITS-1:0]  port_send_dst,   // target ID
    input  wire [NP_W-1:0]          port_take_valid, // doorbell delivered
    input  wire [NP_W*ID_BITS-1:0]  port_take_src,   // its source ID
    output wire [NP_W-1:0]          port_db_valid,   // a doorbell is pending
    output wire [NP_W*ID_BITS-1:0]  port_db_src,     // lowest pending source

    // === External flag per core: a chiplet's doorbell is pending ===
    output wire [N_GLOBAL_CORES-1:0] ef_out
);

    assign ack = 1'b1;  // single-cycle MMIO

    // ========================================================================
    // Mailbox registers
    // ========================================================================
    // Participants are the cores, then the chiplet ports.  Each has a
    // pending mask with one bit per participant that has signalled it.
    localparam NC = N_GLOBAL_CORES;          // core participants
    localparam P  = NC + N_PORTS;
    reg [63:0] mbox_data    [0:NC-1];        // outgoing data per core
    reg [P-1:0] mbox_pending [0:P-1];        // pending signals per target

    localparam integer PORT_ID_BASE_I = {{(32-ID_BITS){1'b0}}, PORT_ID_BASE};

    // An ID names a participant when it is a core's or a port's.
    function id_is_participant;
        input [ID_BITS-1:0] id;
        begin
            id_is_participant =
                ({{(32-ID_BITS){1'b0}}, id} < NC) ||
                (N_PORTS > 0 &&
                 {{(32-ID_BITS){1'b0}}, id} >= PORT_ID_BASE_I &&
                 {{(32-ID_BITS){1'b0}}, id} - PORT_ID_BASE_I < N_PORTS);
        end
    endfunction

    function [31:0] participant_of;
        input [ID_BITS-1:0] id;
        begin
            participant_of =
                ({{(32-ID_BITS){1'b0}}, id} < NC)
                ? {{(32-ID_BITS){1'b0}}, id}
                : NC + ({{(32-ID_BITS){1'b0}}, id} - PORT_ID_BASE_I);
        end
    endfunction

    // Each ID that a write names is decoded here, outside the clocked
    // block, so the pending masks are indexed by plain wires.
    localparam integer PIDX_W = (P <= 1) ? 1 : $clog2(P);

    wire              mmio_id_ok = id_is_participant(wdata[ID_BITS-1:0]);
    wire [31:0]       mmio_pidx_w = participant_of(wdata[ID_BITS-1:0]);
    wire [PIDX_W-1:0] mmio_pidx  = mmio_pidx_w[PIDX_W-1:0];

    wire [N_CORES-1:0] csr_id_ok;
    wire [PIDX_W-1:0]  csr_pidx [0:N_CORES-1];
    wire [NP_W-1:0]    take_id_ok;
    wire [PIDX_W-1:0]  take_pidx [0:NP_W-1];
    wire [NP_W-1:0]    send_id_ok;
    wire [PIDX_W-1:0]  send_pidx [0:NP_W-1];

    genvar di;
    generate
        for (di = 0; di < N_CORES; di = di + 1) begin : csr_id_gen
            wire [31:0] w = participant_of(csr_ipi_wdata[di*64 +: ID_BITS]);
            assign csr_id_ok[di] =
                (csr_ipi_wdata[di*64 + ID_BITS +: 64 - ID_BITS] == 0) &&
                id_is_participant(csr_ipi_wdata[di*64 +: ID_BITS]);
            assign csr_pidx[di] = w[PIDX_W-1:0];
        end
        for (di = 0; di < NP_W; di = di + 1) begin : port_id_gen
            wire [31:0] tw = participant_of(port_take_src[di*ID_BITS +: ID_BITS]);
            wire [31:0] sw = participant_of(port_send_dst[di*ID_BITS +: ID_BITS]);
            assign take_id_ok[di] =
                id_is_participant(port_take_src[di*ID_BITS +: ID_BITS]);
            assign take_pidx[di]  = tw[PIDX_W-1:0];
            assign send_id_ok[di] =
                id_is_participant(port_send_dst[di*ID_BITS +: ID_BITS]);
            assign send_pidx[di]  = sw[PIDX_W-1:0];
        end
    endgenerate

    // IPI output = any pending bit set for that core.  The external flag
    // follows the bits set by chiplet ports alone.
    genvar ci;
    generate
        for (ci = 0; ci < NC; ci = ci + 1) begin : ipi_gen
            assign ipi_out[ci] = |mbox_pending[ci];
            if (N_PORTS > 0) begin : g_ef
                assign ef_out[ci] = |mbox_pending[ci][P-1:NC];
            end else begin : g_no_ef
                assign ef_out[ci] = 1'b0;
            end
        end
    endgenerate

    // Each port sees its lowest pending source, as an ID.
    genvar pg;
    generate
        for (pg = 0; pg < NP_W; pg = pg + 1) begin : port_db_gen
            if (N_PORTS > 0) begin : g_port
                reg [ID_BITS-1:0] low_src;
                integer           li;
                integer           lv;
                always @(*) begin
                    low_src = {ID_BITS{1'b0}};
                    lv      = 0;
                    for (li = P - 1; li >= 0; li = li - 1)
                        if (mbox_pending[NC + pg][li]) begin
                            lv = (li < NC)
                                ? li : PORT_ID_BASE_I + (li - NC);
                            low_src = lv[ID_BITS-1:0];
                        end
                end
                assign port_db_valid[pg] = |mbox_pending[NC + pg];
                assign port_db_src[pg*ID_BITS +: ID_BITS] = low_src;
            end else begin : g_no_port
                assign port_db_valid[pg] = 1'b0;
                assign port_db_src[pg*ID_BITS +: ID_BITS] = {ID_BITS{1'b0}};
            end
        end
    endgenerate

    // ========================================================================
    // CSR-side IPI interface (per-core, no arbiter needed)
    // ========================================================================
    // CSR_MBOX (0x22) read  → returns pending mask for that core
    // CSR_MBOX (0x22) write → send IPI (wdata[ID_BITS-1:0] = target core)
    // CSR_IPIACK (0x23) write → ack IPI (wdata[ID_BITS-1:0] = source core)
    // Each core's pending mask, indexed by ID.  Every core reads the low
    // byte of its mask in MBOX_STATUS; a full core reads all of it by CSR.
    wire [63:0] id_mask [0:NC-1];
    generate
        for (ci = 0; ci < NC; ci = ci + 1) begin : id_mask_gen
            reg [63:0] m;
            integer    mk;
            always @(*) begin
                m = 64'd0;
                for (mk = 0; mk < NC; mk = mk + 1)
                    if (mk < 64)
                        m[mk] = mbox_pending[ci][mk];
                for (mk = 0; mk < N_PORTS; mk = mk + 1)
                    if (PORT_ID_BASE_I + mk < 64)
                        m[PORT_ID_BASE_I + mk] =
                            mbox_pending[ci][NC + mk];
            end
            assign id_mask[ci] = m;
            if (ci < N_CORES) begin : g_csr
                assign csr_ipi_rdata[ci*64 +: 64] = m;
            end
        end
    endgenerate

    // Address decode within MBOX region
    wire is_mbox  = (addr[11:8] == 4'h5);  // MBOX_BASE = 0x500
    wire is_slock = (addr[11:6] == 6'b011000); // 0x600..0x63F
    wire requester_in_mailbox_range = requester_valid &&
                                      requester_id < NC;
    wire requester_in_global_range = requester_valid &&
                                     requester_id < N_GLOBAL_CORES;

    // Mailbox sub-register select (per requester)
    wire [3:0] mbox_reg = addr[3:0];

    // ========================================================================
    // Hardware spinlocks
    // ========================================================================
    reg [N_GLOBAL_CORES-1:0]
                       slock_held  [0:MP64_NUM_SPINLOCKS-1]; // owner bit mask
    reg                slock_locked[0:MP64_NUM_SPINLOCKS-1]; // is lock held?
    reg [ID_BITS-1:0]  slock_owner[0:MP64_NUM_SPINLOCKS-1]; // owning core

    // Spinlock index from address: lock N at offset N*4
    wire [3:0] slock_idx = addr[5:2];
    wire [1:0] slock_reg = addr[1:0];

    // ========================================================================
    // Read mux
    // ========================================================================
    integer ri;
    always @(*) begin
        rdata = 8'd0;

        if (is_mbox) begin
            if (requester_in_mailbox_range) begin
                case (mbox_reg)
                    MBOX_DATA_LO: rdata = mbox_data[requester_id][7:0];
                    4'h1:         rdata = mbox_data[requester_id][15:8];
                    4'h2:         rdata = mbox_data[requester_id][23:16];
                    4'h3:         rdata = mbox_data[requester_id][31:24];
                    MBOX_DATA_HI: rdata = mbox_data[requester_id][39:32];
                    4'h5:         rdata = mbox_data[requester_id][47:40];
                    4'h6:         rdata = mbox_data[requester_id][55:48];
                    4'h7:         rdata = mbox_data[requester_id][63:56];
                    MBOX_STATUS: begin
                        // The architectural status register is eight bits
                        // whatever the core count: the pending bits of IDs
                        // 0 to 7.
                        rdata = id_mask[requester_id][7:0];
                    end
                    default:      rdata = 8'd0;
                endcase
            end
        end else if (is_slock) begin
            // Read SLOCK_ACQUIRE: returns 0=acquired, 1=busy
            // (actual acquisition happens in sequential block below)
            if (slock_reg == SLOCK_ACQUIRE[1:0]) begin
                rdata = {7'd0,
                         !requester_in_global_range ||
                         (slock_locked[slock_idx] &&
                          slock_owner[slock_idx] != requester_id)};
            end
        end
    end

    // ========================================================================
    // Write logic (sequential)
    // ========================================================================
    integer wi;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            for (wi = 0; wi < NC; wi = wi + 1)
                mbox_data[wi]    <= 64'd0;
            for (wi = 0; wi < P; wi = wi + 1)
                mbox_pending[wi] <= {P{1'b0}};
            for (wi = 0; wi < MP64_NUM_SPINLOCKS; wi = wi + 1) begin
                slock_locked[wi] <= 1'b0;
                slock_held[wi]   <= {N_GLOBAL_CORES{1'b0}};
                slock_owner[wi]  <= {ID_BITS{1'b0}};
            end
        end else begin

            // --- Mailbox writes ---
            if (req && wen && is_mbox && requester_in_mailbox_range) begin
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

                    // ACK: write source ID → clear pending bit from that source
                    MBOX_ACK: begin
                        if (mmio_id_ok)
                            mbox_pending[requester_id][mmio_pidx] <= 1'b0;
                    end
                    default: ;
                endcase
            end

            // --- Spinlock acquire (on read of SLOCK_ACQUIRE) ---
            if (req && !wen && is_slock && requester_in_global_range &&
                (slock_reg == SLOCK_ACQUIRE[1:0])) begin
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
            if (req && wen && is_slock && requester_in_global_range &&
                (slock_reg == SLOCK_RELEASE[1:0])) begin
                if (slock_locked[slock_idx] &&
                    slock_owner[slock_idx] == requester_id) begin
                    slock_locked[slock_idx] <= 1'b0;
                    slock_held[slock_idx][requester_id] <= 1'b0;
                end
            end

            // --- CSR-side IPI ack (per-core, bypasses bus arbiter) ---
            for (wi = 0; wi < N_CORES; wi = wi + 1) begin
                if (csr_ipi_wen[wi] &&
                    csr_ipi_addr[wi*8 +: 8] == CSR_IPIACK && csr_id_ok[wi])
                    mbox_pending[wi][csr_pidx[wi]] <= 1'b0;
            end

            // --- A port's bridge delivered a doorbell: clear its source ---
            for (wi = 0; wi < N_PORTS; wi = wi + 1) begin
                if (port_take_valid[wi] && take_id_ok[wi])
                    mbox_pending[NC + wi][take_pidx[wi]] <= 1'b0;
            end

            // Every send follows every clear, so a signal raised in the
            // cycle its predecessor is cleared stays pending.

            // --- SEND: write target ID → set pending bit on target ---
            if (req && wen && is_mbox && requester_in_mailbox_range &&
                mbox_reg == MBOX_SEND && mmio_id_ok &&
                {{(32-PIDX_W){1'b0}}, mmio_pidx} !=
                {{(32-ID_BITS){1'b0}}, requester_id})
                mbox_pending[mmio_pidx][requester_id] <= 1'b1;

            // --- CSR-side IPI send ---
            for (wi = 0; wi < N_CORES; wi = wi + 1) begin
                if (csr_ipi_wen[wi] &&
                    csr_ipi_addr[wi*8 +: 8] == CSR_MBOX && csr_id_ok[wi] &&
                    {{(32-PIDX_W){1'b0}}, csr_pidx[wi]} != wi)
                    mbox_pending[csr_pidx[wi]][wi] <= 1'b1;
            end

            // --- A port rings a participant ---
            for (wi = 0; wi < N_PORTS; wi = wi + 1) begin
                if (port_send_valid[wi] && send_id_ok[wi] &&
                    {{(32-PIDX_W){1'b0}}, send_pidx[wi]} != NC + wi)
                    mbox_pending[send_pidx[wi]][NC + wi] <= 1'b1;
            end
        end
    end

endmodule
