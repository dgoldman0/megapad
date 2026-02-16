// ============================================================================
// mp64_bus.v — Multi-Master Bus Arbiter & Address Decoder
// ============================================================================
//
// Routes requests from N_PORTS CPU masters to two targets:
//   - Memory subsystem (internal BRAM + external forwarding)
//   - MMIO peripherals (UART, Timer, Disk, NIC, Mailbox, Spinlocks)
//
// Arbitration: weighted round-robin with per-core bandwidth limiting.
//   - Each core has a weight (1-255): core gets `weight` consecutive grants
//     before the arbiter advances to the next requesting core.
//   - Each core has a bandwidth limit: beats per epoch. Once exceeded,
//     the core is throttled until the epoch resets.
//
// QoS CSRs (written from CPU via sideband):
//   CSR_QOS_WEIGHT  (0x58) — per-core weights packed [7:0] per core
//   CSR_QOS_BWLIMIT (0x59) — per-core BW limits packed [15:0] per core
//
// Protocol (3-state FSM):
//   1. IDLE      — pick next requesting core (weighted RR), register request
//   2. MMIO_RESP — capture mmio_rdata, assert cpu_ready, return to IDLE
//   3. MEM_WAIT  — hold mem_req; wait for mem_ack, capture mem_rdata
//

`include "mp64_defs.vh"

module mp64_bus #(
    parameter N_PORTS   = NUM_CORES,    // number of bus master ports
    parameter PORT_BITS = $clog2(N_PORTS > 1 ? N_PORTS : 2) // grant register width
) (
    input  wire        clk,
    input  wire        rst_n,

    // === CPU master ports (flat-packed arrays, N_PORTS wide) ===
    input  wire [N_PORTS-1:0]          cpu_valid,
    input  wire [N_PORTS*64-1:0]       cpu_addr,
    input  wire [N_PORTS*64-1:0]       cpu_wdata,
    input  wire [N_PORTS-1:0]          cpu_wen,
    input  wire [N_PORTS*2-1:0]        cpu_size,
    output reg  [N_PORTS*64-1:0]       cpu_rdata,
    output reg  [N_PORTS-1:0]          cpu_ready,

    // === Memory subsystem (BRAM + external forwarding) ===
    output reg         mem_req,
    output reg  [63:0] mem_addr,
    output reg  [63:0] mem_wdata,
    output reg         mem_wen,
    output reg  [1:0]  mem_size,
    input  wire [63:0] mem_rdata,
    input  wire        mem_ack,

    // === MMIO peripheral bus ===
    output reg         mmio_req,
    output reg  [11:0] mmio_addr,
    output reg  [63:0] mmio_wdata,
    output reg         mmio_wen,
    output reg  [1:0]  mmio_size,
    input  wire [63:0] mmio_rdata,
    input  wire        mmio_ack,

    // === QoS CSR sideband (from any core's CSR write) ===
    input  wire        qos_csr_wen,
    input  wire [7:0]  qos_csr_addr,
    input  wire [63:0] qos_csr_wdata,
    output reg  [63:0] qos_csr_rdata
);

    // ========================================================================
    // Arbiter FSM states
    // ========================================================================
    localparam [1:0] ARB_IDLE      = 2'd0;
    localparam [1:0] ARB_MEM_WAIT  = 2'd1;
    localparam [1:0] ARB_MMIO_RESP = 2'd2;

    reg [1:0]              arb_state;
    reg [PORT_BITS-1:0]    grant;       // currently granted port
    reg [PORT_BITS-1:0]    last_grant;  // round-robin seed
    reg                    served_last; // prevents stale re-serve of same core

    // ========================================================================
    // Unpack per-core signals for readability
    // ========================================================================
    wire [63:0] core_addr  [0:N_PORTS-1];
    wire [63:0] core_wdata [0:N_PORTS-1];
    wire [1:0]  core_size  [0:N_PORTS-1];

    genvar gi;
    generate
        for (gi = 0; gi < N_PORTS; gi = gi + 1) begin : unpack
            assign core_addr[gi]  = cpu_addr [gi*64 +: 64];
            assign core_wdata[gi] = cpu_wdata[gi*64 +: 64];
            assign core_size[gi]  = cpu_size [gi*2  +: 2];
        end
    endgenerate

    // ========================================================================
    // Round-robin next-grant selection (weighted, with BW throttling)
    // ========================================================================

    // QoS registers (sideband CSR writes)
    reg [7:0]  qos_weight [0:N_PORTS-1];  // Grants before advancing
    reg [15:0] qos_bwlimit[0:N_PORTS-1]; // Max beats per epoch (0=unlimited)
    reg [15:0] qos_bw_cnt [0:N_PORTS-1]; // Current epoch beat count
    reg [7:0]  weight_remain;               // Remaining grants for current core
    reg [15:0] epoch_timer;                 // Epoch timer (65536 cycles)

    // QoS CSR write handling
    integer qi;
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            for (qi = 0; qi < N_PORTS; qi = qi + 1) begin
                qos_weight[qi]  <= 8'd1;   // Default: equal weight
                qos_bwlimit[qi] <= 16'd0;  // Default: unlimited
                qos_bw_cnt[qi]  <= 16'd0;
            end
            epoch_timer <= 16'd0;
            qos_csr_rdata <= 64'd0;
        end else begin
            // Epoch timer: reset BW counters every 65536 cycles
            epoch_timer <= epoch_timer + 16'd1;
            if (epoch_timer == 16'hFFFF) begin
                for (qi = 0; qi < N_PORTS; qi = qi + 1)
                    qos_bw_cnt[qi] <= 16'd0;
            end

            // CSR writes — packed 8 bits/port for weight, 16 bits/port for BW
            if (qos_csr_wen) begin
                case (qos_csr_addr)
                    CSR_QOS_WEIGHT: begin
                        // Pack: bits [7:0]=port0, [15:8]=port1, etc. (up to 8 ports)
                        for (qi = 0; qi < N_PORTS && qi < 8; qi = qi + 1) begin
                            qos_weight[qi] <= (qos_csr_wdata[qi*8 +: 8] == 8'd0)
                                ? 8'd1 : qos_csr_wdata[qi*8 +: 8];
                        end
                    end
                    CSR_QOS_BWLIMIT: begin
                        // Pack: bits [15:0]=port0, [31:16]=port1, etc. (up to 4 ports)
                        for (qi = 0; qi < N_PORTS && qi < 4; qi = qi + 1)
                            qos_bwlimit[qi] <= qos_csr_wdata[qi*16 +: 16];
                    end
                    default: ;
                endcase
            end

            // CSR read mux
            begin : qos_read_mux
                reg [63:0] rd_weight, rd_bwlimit;
                rd_weight  = 64'd0;
                rd_bwlimit = 64'd0;
                for (qi = 0; qi < N_PORTS && qi < 8; qi = qi + 1)
                    rd_weight[qi*8 +: 8] = qos_weight[qi];
                for (qi = 0; qi < N_PORTS && qi < 4; qi = qi + 1)
                    rd_bwlimit[qi*16 +: 16] = qos_bwlimit[qi];
                case (qos_csr_addr)
                    CSR_QOS_WEIGHT:  qos_csr_rdata <= rd_weight;
                    CSR_QOS_BWLIMIT: qos_csr_rdata <= rd_bwlimit;
                    default:         qos_csr_rdata <= 64'd0;
                endcase
            end
        end
    end

    // BW-throttled eligibility: a port can request if BW limit not exceeded
    wire [N_PORTS-1:0] eligible;
    genvar ei;
    generate
        for (ei = 0; ei < N_PORTS; ei = ei + 1) begin : elig
            assign eligible[ei] = cpu_valid[ei] &&
                (qos_bwlimit[ei] == 16'd0 || qos_bw_cnt[ei] < qos_bwlimit[ei]);
        end
    endgenerate

    reg [PORT_BITS-1:0] next_grant;
    reg                 any_request;

    // Weighted round-robin: if current port still has weight remaining
    // and is eligible, keep granting it. Otherwise scan next eligible port.
    integer rr_i;
    always @(*) begin
        next_grant  = last_grant;
        any_request = 1'b0;

        // If current port still has weight budget and is requesting
        if (weight_remain > 8'd0 && eligible[last_grant]) begin
            next_grant  = last_grant;
            any_request = 1'b1;
        end else begin
            // Scan for next eligible port (round-robin, N_PORTS iteration)
            for (rr_i = 1; rr_i <= N_PORTS; rr_i = rr_i + 1) begin
                if (!any_request &&
                    eligible[(last_grant + rr_i[PORT_BITS-1:0]) % N_PORTS])
                begin
                    next_grant  = (last_grant + rr_i[PORT_BITS-1:0]) % N_PORTS;
                    any_request = 1'b1;
                end
            end
        end
    end

    // ========================================================================
    // Main arbiter FSM
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            arb_state  <= ARB_IDLE;
            grant      <= {PORT_BITS{1'b0}};
            last_grant <= {PORT_BITS{1'b0}};
            served_last<= 1'b0;
            weight_remain <= 8'd1;
            mem_req    <= 1'b0;
            mmio_req   <= 1'b0;
            cpu_ready  <= {N_PORTS{1'b0}};
            cpu_rdata  <= {(N_PORTS*64){1'b0}};
            mem_addr   <= 64'd0;
            mem_wdata  <= 64'd0;
            mem_wen    <= 1'b0;
            mem_size   <= 2'd0;
            mmio_addr  <= 12'd0;
            mmio_wdata <= 64'd0;
            mmio_wen   <= 1'b0;
            mmio_size  <= 2'd0;
        end else begin
            // Default: deassert ready pulse each cycle
            cpu_ready <= {N_PORTS{1'b0}};

            case (arb_state)
                // --------------------------------------------------------
                // IDLE — pick next requesting core, register request
                // --------------------------------------------------------
                ARB_IDLE: begin
                    mem_req  <= 1'b0;
                    mmio_req <= 1'b0;

                    if (any_request &&
                        !(served_last && next_grant == last_grant)) begin
                        grant      <= next_grant;
                        served_last<= 1'b0;

                        // Update weight counter
                        if (next_grant == last_grant && weight_remain > 8'd0) begin
                            weight_remain <= weight_remain - 8'd1;
                        end else begin
                            weight_remain <= qos_weight[next_grant] - 8'd1;
                        end

                        if (core_addr[next_grant][63:32] == MMIO_HI) begin
                            // MMIO: register request, respond next cycle
                            mmio_req   <= 1'b1;
                            mmio_addr  <= core_addr[next_grant][11:0];
                            mmio_wdata <= core_wdata[next_grant];
                            mmio_wen   <= cpu_wen[next_grant];
                            mmio_size  <= core_size[next_grant];
                            arb_state  <= ARB_MMIO_RESP;
                        end else begin
                            // Memory: register request, wait for ack
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

                // --------------------------------------------------------
                // MMIO_RESP — mmio_addr stable → mmio_rdata valid
                // --------------------------------------------------------
                ARB_MMIO_RESP: begin
                    cpu_rdata[grant*64 +: 64] <= mmio_rdata;
                    cpu_ready[grant]          <= 1'b1;
                    last_grant                <= grant;
                    mmio_req                  <= 1'b0;
                    served_last               <= 1'b1;
                    qos_bw_cnt[grant]         <= qos_bw_cnt[grant] + 16'd1;
                    arb_state                 <= ARB_IDLE;
                end

                // --------------------------------------------------------
                // MEM_WAIT — hold mem_req until memory acks
                // --------------------------------------------------------
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
                    // else: mem_req, mem_addr, etc. held stable from IDLE
                end

                default: arb_state <= ARB_IDLE;
            endcase
        end
    end

endmodule
