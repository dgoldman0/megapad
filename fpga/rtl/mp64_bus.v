// ============================================================================
// mp64_bus.v — Multi-Master Bus Arbiter & Address Decoder
// ============================================================================
//
// Routes requests from NUM_CORES CPU masters to two targets:
//   - Memory subsystem (internal BRAM + external forwarding)
//   - MMIO peripherals (UART, Timer, Disk, NIC, Mailbox, Spinlocks)
//
// Arbitration: round-robin among requesting cores with fairness.
// One core owns the bus at a time.  Other cores stall (bus_ready low).
//
// Protocol (3-state FSM):
//   1. IDLE      — pick next requesting core (round-robin), register request
//                  to target bus, advance to MMIO_RESP or MEM_WAIT.
//   2. MMIO_RESP — mmio_addr now stable; capture combinational mmio_rdata,
//                  assert cpu_ready, return to IDLE.
//   3. MEM_WAIT  — hold mem_req; wait for mem_ack, capture mem_rdata,
//                  assert cpu_ready, return to IDLE.
//
// Latency:
//   MMIO:   2 arbiter cycles (register → capture).
//   Memory: 3+ arbiter cycles (register → BRAM latency → capture).
//

`include "mp64_defs.vh"

module mp64_bus (
    input  wire        clk,
    input  wire        rst_n,

    // === CPU master ports (flat-packed arrays, NUM_CORES wide) ===
    input  wire [NUM_CORES-1:0]        cpu_valid,
    input  wire [NUM_CORES*64-1:0]     cpu_addr,
    input  wire [NUM_CORES*64-1:0]     cpu_wdata,
    input  wire [NUM_CORES-1:0]        cpu_wen,
    input  wire [NUM_CORES*2-1:0]      cpu_size,
    output reg  [NUM_CORES*64-1:0]     cpu_rdata,
    output reg  [NUM_CORES-1:0]        cpu_ready,

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
    input  wire        mmio_ack
);

    // ========================================================================
    // Arbiter FSM states
    // ========================================================================
    localparam [1:0] ARB_IDLE      = 2'd0;
    localparam [1:0] ARB_MEM_WAIT  = 2'd1;
    localparam [1:0] ARB_MMIO_RESP = 2'd2;

    reg [1:0]              arb_state;
    reg [CORE_ID_BITS-1:0] grant;       // currently granted core
    reg [CORE_ID_BITS-1:0] last_grant;  // round-robin seed
    reg                    served_last; // prevents stale re-serve of same core

    // ========================================================================
    // Unpack per-core signals for readability
    // ========================================================================
    wire [63:0] core_addr  [0:NUM_CORES-1];
    wire [63:0] core_wdata [0:NUM_CORES-1];
    wire [1:0]  core_size  [0:NUM_CORES-1];

    genvar gi;
    generate
        for (gi = 0; gi < NUM_CORES; gi = gi + 1) begin : unpack
            assign core_addr[gi]  = cpu_addr [gi*64 +: 64];
            assign core_wdata[gi] = cpu_wdata[gi*64 +: 64];
            assign core_size[gi]  = cpu_size [gi*2  +: 2];
        end
    endgenerate

    // ========================================================================
    // Round-robin next-grant selection
    // ========================================================================
    reg [CORE_ID_BITS-1:0] next_grant;
    reg                     any_request;

    // Scan starting from last_grant+1, wrap around (unrolled for 4 cores).
    always @(*) begin
        next_grant  = last_grant;
        any_request = 1'b0;

        if (cpu_valid[(last_grant + 2'd1) & 2'd3]) begin
            next_grant  = (last_grant + 2'd1) & 2'd3;
            any_request = 1'b1;
        end else if (cpu_valid[(last_grant + 2'd2) & 2'd3]) begin
            next_grant  = (last_grant + 2'd2) & 2'd3;
            any_request = 1'b1;
        end else if (cpu_valid[(last_grant + 2'd3) & 2'd3]) begin
            next_grant  = (last_grant + 2'd3) & 2'd3;
            any_request = 1'b1;
        end else if (cpu_valid[last_grant]) begin
            next_grant  = last_grant;
            any_request = 1'b1;
        end
    end

    // ========================================================================
    // Main arbiter FSM
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            arb_state  <= ARB_IDLE;
            grant      <= {CORE_ID_BITS{1'b0}};
            last_grant <= {CORE_ID_BITS{1'b0}};
            served_last<= 1'b0;
            mem_req    <= 1'b0;
            mmio_req   <= 1'b0;
            cpu_ready  <= {NUM_CORES{1'b0}};
            cpu_rdata  <= {(NUM_CORES*64){1'b0}};
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
            cpu_ready <= {NUM_CORES{1'b0}};

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
                        arb_state                 <= ARB_IDLE;
                    end
                    // else: mem_req, mem_addr, etc. held stable from IDLE
                end

                default: arb_state <= ARB_IDLE;
            endcase
        end
    end

endmodule
