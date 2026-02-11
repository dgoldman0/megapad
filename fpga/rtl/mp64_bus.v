// ============================================================================
// mp64_bus.v — Multi-Master Bus Arbiter & Address Decoder
// ============================================================================
//
// Routes requests from NUM_CORES CPU masters to two targets:
//   - Memory subsystem (internal BRAM + external forwarding)
//   - MMIO peripherals (UART, Timer, Disk, NIC, Mailbox, Spinlocks)
//
// Arbitration: round-robin among requesting cores with fairness.
// One core owns the bus per cycle.  Other cores stall (bus_ready low).
//
// MMIO accesses are single-cycle (combinational ack).
// Memory accesses take 1+ cycles (registered ack from BRAM/ext).
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
    // Arbiter state
    // ========================================================================
    reg [CORE_ID_BITS-1:0] grant;       // currently granted core
    reg [CORE_ID_BITS-1:0] last_grant;  // last core granted (round-robin seed)
    reg                     bus_busy;    // memory transaction in flight

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
    integer i;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            grant      <= {CORE_ID_BITS{1'b0}};
            last_grant <= {CORE_ID_BITS{1'b0}};
            bus_busy   <= 1'b0;
            mem_req    <= 1'b0;
            mmio_req   <= 1'b0;
            cpu_ready  <= {NUM_CORES{1'b0}};
            cpu_rdata  <= {(NUM_CORES*64){1'b0}};
        end else begin
            // Default: deassert all handshake signals each cycle
            cpu_ready <= {NUM_CORES{1'b0}};
            mem_req   <= 1'b0;
            mmio_req  <= 1'b0;

            if (bus_busy) begin
                // --------------------------------------------------------
                // Memory transaction in progress — wait for ack
                // --------------------------------------------------------
                if (mem_ack) begin
                    cpu_rdata[grant*64 +: 64] <= mem_rdata;
                    cpu_ready[grant]          <= 1'b1;
                    bus_busy                  <= 1'b0;
                    last_grant                <= grant;
                end else begin
                    // Hold the request for the granted core
                    mem_req   <= 1'b1;
                    mem_addr  <= core_addr[grant];
                    mem_wdata <= core_wdata[grant];
                    mem_wen   <= cpu_wen[grant];
                    mem_size  <= core_size[grant];
                end
            end else begin
                // --------------------------------------------------------
                // Bus idle — pick next requesting core (round-robin)
                // --------------------------------------------------------
                if (any_request) begin
                    grant <= next_grant;

                    if (core_addr[next_grant][63:32] == MMIO_HI) begin
                        // MMIO access — single-cycle response
                        mmio_req   <= 1'b1;
                        mmio_addr  <= core_addr[next_grant][11:0];
                        mmio_wdata <= core_wdata[next_grant];
                        mmio_wen   <= cpu_wen[next_grant];
                        mmio_size  <= core_size[next_grant];
                        // Combinational ack — complete same cycle
                        cpu_rdata[next_grant*64 +: 64] <= mmio_rdata;
                        cpu_ready[next_grant]          <= mmio_ack;
                        last_grant                     <= next_grant;
                    end else begin
                        // Memory access — may take 1+ cycles
                        mem_req   <= 1'b1;
                        mem_addr  <= core_addr[next_grant];
                        mem_wdata <= core_wdata[next_grant];
                        mem_wen   <= cpu_wen[next_grant];
                        mem_size  <= core_size[next_grant];
                        if (mem_ack) begin
                            // BRAM can respond same cycle
                            cpu_rdata[next_grant*64 +: 64] <= mem_rdata;
                            cpu_ready[next_grant]          <= 1'b1;
                            last_grant                     <= next_grant;
                        end else begin
                            bus_busy <= 1'b1;
                        end
                    end
                end
            end
        end
    end

endmodule
