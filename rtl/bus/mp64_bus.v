// ============================================================================
// mp64_bus.v — Multi-Master Bus Arbiter & Address Decoder
// ============================================================================
//
// Routes requests from N_PORTS CPU masters to two targets:
//   - Memory subsystem (internal BRAM + external forwarding)
//   - MMIO peripherals (decoded by upper address bits)
//
// Arbitration: weighted round-robin with per-core bandwidth limiting.
//   - Each port has a weight (1–255): granted `weight` consecutive beats
//     before the arbiter advances to the next requesting port.
//   - Each port has a bandwidth limit: beats per epoch (65536 cycles).
//     Once exceeded the port is throttled until the epoch resets.
//
// QoS CSRs (sideband write from any core):
//   CSR_QOS_WEIGHT  — packed [7:0] per port (up to 8 ports)
//   CSR_QOS_BWLIMIT — packed [15:0] per port (up to 4 ports)
//
// Protocol (3-state FSM):
//   IDLE      — pick next eligible requesting port; register request
//   MMIO_RESP — capture mmio_rdata, assert cpu_ready, return to IDLE
//   MEM_WAIT  — hold mem_req; wait for mem_ack, capture mem_rdata
//
// Coding standard:
//   Verilog-2001 · sync reset · non-blocking assigns · no `%` operator
//

module mp64_bus #(
    parameter N_PORTS   = 4,
    parameter PORT_BITS = 3  // ceil(log2(N_PORTS)), must hold 0..N_PORTS-1
)(
    input  wire        clk,
    input  wire        rst_n,

    // === CPU master ports (flat-packed, N_PORTS wide) ===
    input  wire [N_PORTS-1:0]      cpu_valid,
    input  wire [N_PORTS*64-1:0]   cpu_addr,
    input  wire [N_PORTS*64-1:0]   cpu_wdata,
    input  wire [N_PORTS-1:0]      cpu_wen,
    input  wire [N_PORTS*2-1:0]    cpu_size,
    output reg  [N_PORTS*64-1:0]   cpu_rdata,
    output reg  [N_PORTS-1:0]      cpu_ready,

    // === Memory subsystem target ===
    output reg         mem_req,
    output reg  [63:0] mem_addr,
    output reg  [63:0] mem_wdata,
    output reg         mem_wen,
    output reg  [1:0]  mem_size,
    input  wire [63:0] mem_rdata,
    input  wire        mem_ack,

    // === MMIO peripheral target ===
    output reg         mmio_req,
    output reg  [11:0] mmio_addr,
    output reg  [63:0] mmio_wdata,
    output reg         mmio_wen,
    output reg  [1:0]  mmio_size,
    input  wire [63:0] mmio_rdata,
    input  wire        mmio_ack,

    // === QoS CSR sideband ===
    input  wire        qos_csr_wen,
    input  wire [7:0]  qos_csr_addr,
    input  wire [63:0] qos_csr_wdata,
    output reg  [63:0] qos_csr_rdata
);

    `include "mp64_pkg.vh"

    // ========================================================================
    // FSM States
    // ========================================================================
    localparam [1:0] ARB_IDLE      = 2'd0;
    localparam [1:0] ARB_MEM_WAIT  = 2'd1;
    localparam [1:0] ARB_MMIO_RESP = 2'd2;

    reg [1:0]           arb_state;
    reg [PORT_BITS-1:0] grant;
    reg [PORT_BITS-1:0] last_grant;
    reg                 served_last;

    // ========================================================================
    // Unpack per-port signals
    // ========================================================================
    wire [63:0] core_addr  [0:N_PORTS-1];
    wire [63:0] core_wdata [0:N_PORTS-1];
    wire [1:0]  core_size  [0:N_PORTS-1];

    genvar gi;
    generate
        for (gi = 0; gi < N_PORTS; gi = gi + 1) begin : g_unpack
            assign core_addr[gi]  = cpu_addr [gi*64 +: 64];
            assign core_wdata[gi] = cpu_wdata[gi*64 +: 64];
            assign core_size[gi]  = cpu_size [gi*2  +: 2];
        end
    endgenerate

    // ========================================================================
    // QoS Registers
    // ========================================================================
    reg [7:0]  qos_weight  [0:N_PORTS-1];
    reg [15:0] qos_bwlimit [0:N_PORTS-1];
    reg [15:0] qos_bw_cnt  [0:N_PORTS-1];
    reg [7:0]  weight_remain;
    reg [15:0] epoch_timer;

    integer qi;
    always @(posedge clk) begin
        if (!rst_n) begin
            for (qi = 0; qi < N_PORTS; qi = qi + 1) begin
                qos_weight[qi]  <= 8'd1;
                qos_bwlimit[qi] <= 16'd0;
                qos_bw_cnt[qi]  <= 16'd0;
            end
            epoch_timer   <= 16'd0;
            qos_csr_rdata <= 64'd0;
        end else begin
            // Epoch timer — reset BW counters every 65536 cycles
            epoch_timer <= epoch_timer + 16'd1;
            if (epoch_timer == 16'hFFFF) begin
                for (qi = 0; qi < N_PORTS; qi = qi + 1)
                    qos_bw_cnt[qi] <= 16'd0;
            end

            // CSR writes
            if (qos_csr_wen) begin
                case (qos_csr_addr)
                    CSR_QOS_WEIGHT: begin
                        for (qi = 0; qi < N_PORTS && qi < 8; qi = qi + 1)
                            qos_weight[qi] <= (qos_csr_wdata[qi*8 +: 8] == 8'd0)
                                            ? 8'd1
                                            : qos_csr_wdata[qi*8 +: 8];
                    end
                    CSR_QOS_BWLIMIT: begin
                        for (qi = 0; qi < N_PORTS && qi < 4; qi = qi + 1)
                            qos_bwlimit[qi] <= qos_csr_wdata[qi*16 +: 16];
                    end
                    default: ;
                endcase
            end

            // CSR read mux
            begin : qos_rd_blk
                reg [63:0] w_pack, bw_pack;
                w_pack  = 64'd0;
                bw_pack = 64'd0;
                for (qi = 0; qi < N_PORTS && qi < 8; qi = qi + 1)
                    w_pack[qi*8 +: 8] = qos_weight[qi];
                for (qi = 0; qi < N_PORTS && qi < 4; qi = qi + 1)
                    bw_pack[qi*16 +: 16] = qos_bwlimit[qi];
                case (qos_csr_addr)
                    CSR_QOS_WEIGHT:  qos_csr_rdata <= w_pack;
                    CSR_QOS_BWLIMIT: qos_csr_rdata <= bw_pack;
                    default:         qos_csr_rdata <= 64'd0;
                endcase
            end
        end
    end

    // ========================================================================
    // Eligibility (valid + not BW-throttled)
    // ========================================================================
    wire [N_PORTS-1:0] eligible;
    genvar ei;
    generate
        for (ei = 0; ei < N_PORTS; ei = ei + 1) begin : g_elig
            assign eligible[ei] = cpu_valid[ei] &&
                (qos_bwlimit[ei] == 16'd0 || qos_bw_cnt[ei] < qos_bwlimit[ei]);
        end
    endgenerate

    // ========================================================================
    // Weighted Round-Robin Next-Grant
    // ========================================================================
    // Uses iterative scan instead of `%` operator.
    reg [PORT_BITS-1:0] next_grant;
    reg                 any_request;

    integer rr_i;
    reg [PORT_BITS-1:0] rr_candidate;

    always @(*) begin
        next_grant  = last_grant;
        any_request = 1'b0;

        // Current port still has weight budget and is eligible?
        if (weight_remain > 8'd0 && eligible[last_grant]) begin
            next_grant  = last_grant;
            any_request = 1'b1;
        end else begin
            // Scan for next eligible port (round-robin)
            for (rr_i = 1; rr_i <= N_PORTS; rr_i = rr_i + 1) begin
                // Synthesisable wrap-around without %
                rr_candidate = last_grant + rr_i[PORT_BITS-1:0];
                if (rr_candidate >= N_PORTS[PORT_BITS-1:0])
                    rr_candidate = rr_candidate - N_PORTS[PORT_BITS-1:0];
                if (!any_request && eligible[rr_candidate]) begin
                    next_grant  = rr_candidate;
                    any_request = 1'b1;
                end
            end
        end
    end

    // ========================================================================
    // MMIO detection — addr[63:32] == 0xFFFF_FF00
    // ========================================================================
    wire is_mmio = (core_addr[next_grant][63:32] == MP64_MMIO_HI);

    // ========================================================================
    // Main Arbiter FSM
    // ========================================================================
    always @(posedge clk) begin
        if (!rst_n) begin
            arb_state     <= ARB_IDLE;
            grant         <= {PORT_BITS{1'b0}};
            last_grant    <= {PORT_BITS{1'b0}};
            served_last   <= 1'b0;
            weight_remain <= 8'd1;
            mem_req       <= 1'b0;
            mmio_req      <= 1'b0;
            cpu_ready     <= {N_PORTS{1'b0}};
            cpu_rdata     <= {(N_PORTS*64){1'b0}};
            mem_addr      <= 64'd0;
            mem_wdata     <= 64'd0;
            mem_wen       <= 1'b0;
            mem_size      <= 2'd0;
            mmio_addr     <= 12'd0;
            mmio_wdata    <= 64'd0;
            mmio_wen      <= 1'b0;
            mmio_size     <= 2'd0;
        end else begin
            // Default: clear ready pulses each cycle
            cpu_ready <= {N_PORTS{1'b0}};

            case (arb_state)

                // ====================================================
                // IDLE — select next eligible port, register request
                // ====================================================
                ARB_IDLE: begin
                    mem_req  <= 1'b0;
                    mmio_req <= 1'b0;

                    if (any_request &&
                        !(served_last && next_grant == last_grant)) begin

                        grant       <= next_grant;
                        served_last <= 1'b0;

                        // Update weight counter
                        if (next_grant == last_grant && weight_remain > 8'd0)
                            weight_remain <= weight_remain - 8'd1;
                        else
                            weight_remain <= qos_weight[next_grant] - 8'd1;

                        if (is_mmio) begin
                            mmio_req   <= 1'b1;
                            mmio_addr  <= core_addr[next_grant][11:0];
                            mmio_wdata <= core_wdata[next_grant];
                            mmio_wen   <= cpu_wen[next_grant];
                            mmio_size  <= core_size[next_grant];
                            arb_state  <= ARB_MMIO_RESP;
                        end else begin
                            mem_req   <= 1'b1;
                            mem_addr  <= core_addr[next_grant];
                            mem_wdata <= core_wdata[next_grant];
                            mem_wen   <= cpu_wen[next_grant];
                            mem_size  <= core_size[next_grant];
                            arb_state <= ARB_MEM_WAIT;
                        end
                    end else begin
                        served_last <= 1'b0;
                    end
                end

                // ====================================================
                // MMIO_RESP — peripheral responds in 1 cycle
                // ====================================================
                ARB_MMIO_RESP: begin
                    if (mmio_ack) begin
                        cpu_rdata[grant*64 +: 64] <= mmio_rdata;
                        cpu_ready[grant]          <= 1'b1;
                        last_grant                <= grant;
                        mmio_req                  <= 1'b0;
                        served_last               <= 1'b1;
                        qos_bw_cnt[grant]         <= qos_bw_cnt[grant] + 16'd1;
                        arb_state                 <= ARB_IDLE;
                    end
                end

                // ====================================================
                // MEM_WAIT — hold mem_req until memory acks
                // ====================================================
                ARB_MEM_WAIT: begin
                    if (mem_ack) begin
                        cpu_rdata[grant*64 +: 64] <= mem_rdata;
                        cpu_ready[grant]          <= 1'b1;
                        last_grant                <= grant;
                        mem_req                   <= 1'b0;
                        served_last               <= 1'b1;
                        qos_bw_cnt[grant]         <= qos_bw_cnt[grant] + 16'd1;
                        arb_state                 <= ARB_IDLE;
                    end
                end

                default: arb_state <= ARB_IDLE;
            endcase
        end
    end

endmodule
