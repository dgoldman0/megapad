// ============================================================================
// mp64_extmem.v — External Memory Controller
// ============================================================================
//
// Bridges the Megapad-64 internal bus to external memory (HyperRAM or SDRAM).
// Supports two access modes:
//
//   1. CPU access — 64-bit reads/writes forwarded from mp64_memory when
//      address >= INT_MEM_BYTES.  Single-beat transactions.
//
//   2. Tile access — 512-bit reads/writes from the tile engine.
//      Translated into 8-beat bursts of 64 bits each.
//
// The external memory interface uses a generic protocol that can be adapted
// to HyperRAM (via a PHY wrapper) or SDRAM (via a standard controller).
//
// Priority: tile requests preempt CPU requests (tile engine is latency-
// sensitive for pipeline throughput).
//
// Performance:
//   - CPU single read:  ~6-10 cycles (HyperRAM latency)
//   - Tile 512-bit read: ~14-18 cycles (6 latency + 8 burst beats)
//   - Internal BRAM:     1 cycle (for comparison)
//

`include "mp64_defs.vh"

module mp64_extmem (
    input  wire        clk,
    input  wire        rst_n,

    // === CPU port (from mp64_memory external forward) ===
    input  wire        cpu_req,
    input  wire [63:0] cpu_addr,
    input  wire [63:0] cpu_wdata,
    input  wire        cpu_wen,
    input  wire [1:0]  cpu_size,
    output reg  [63:0] cpu_rdata,
    output reg         cpu_ack,

    // === Tile port (from mp64_tile external tile requests) ===
    input  wire        tile_req,
    input  wire [63:0] tile_addr,      // 64B aligned
    input  wire        tile_wen,
    input  wire [511:0]tile_wdata,
    output reg  [511:0]tile_rdata,
    output reg         tile_ack,

    // === External PHY interface (directly to pins) ===
    // Generic burst-capable interface — adapt to HyperRAM or SDRAM PHY
    output reg         phy_req,
    output reg  [31:0] phy_addr,       // 32-bit external address space
    output reg         phy_wen,
    output reg  [63:0] phy_wdata,
    output reg  [3:0]  phy_burst_len,  // 0=single, 1-15=burst
    input  wire [63:0] phy_rdata,
    input  wire        phy_rvalid,     // read data valid (one per beat)
    input  wire        phy_ready       // PHY ready for next command
);

    // ========================================================================
    // State machine
    // ========================================================================
    localparam S_IDLE        = 4'd0;
    localparam S_CPU_SINGLE  = 4'd1;
    localparam S_CPU_WAIT    = 4'd2;
    localparam S_TILE_BURST  = 4'd3;
    localparam S_TILE_READ   = 4'd4;
    localparam S_TILE_WRITE  = 4'd5;

    reg [3:0]  state;
    reg [2:0]  beat_cnt;       // 0-7 for 8-beat bursts
    reg [511:0] tile_buf;      // assemble/disassemble 512 bits
    reg        tile_is_write;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state     <= S_IDLE;
            cpu_ack   <= 1'b0;
            tile_ack  <= 1'b0;
            phy_req   <= 1'b0;
            beat_cnt  <= 3'd0;
        end else begin
            cpu_ack  <= 1'b0;
            tile_ack <= 1'b0;
            phy_req  <= 1'b0;

            case (state)
                // --------------------------------------------------------
                S_IDLE: begin
                    // Tile requests have priority
                    if (tile_req && phy_ready) begin
                        tile_is_write <= tile_wen;
                        if (tile_wen) begin
                            // Write: send 8 beats from tile_wdata
                            tile_buf <= tile_wdata;
                            beat_cnt <= 3'd0;
                            state    <= S_TILE_WRITE;
                        end else begin
                            // Read: issue burst read command
                            phy_req       <= 1'b1;
                            phy_addr      <= tile_addr[31:0];
                            phy_wen       <= 1'b0;
                            phy_burst_len <= 4'd8;  // 8 × 64-bit = 512 bits
                            beat_cnt      <= 3'd0;
                            state         <= S_TILE_READ;
                        end
                    end else if (cpu_req && phy_ready) begin
                        // CPU single-beat access
                        phy_req       <= 1'b1;
                        phy_addr      <= cpu_addr[31:0];
                        phy_wen       <= cpu_wen;
                        phy_wdata     <= cpu_wdata;
                        phy_burst_len <= 4'd0;  // single beat
                        state         <= S_CPU_WAIT;
                    end
                end

                // --------------------------------------------------------
                S_CPU_WAIT: begin
                    if (phy_rvalid || (cpu_wen && phy_ready)) begin
                        cpu_rdata <= phy_rdata;
                        cpu_ack   <= 1'b1;
                        state     <= S_IDLE;
                    end
                end

                // --------------------------------------------------------
                S_TILE_READ: begin
                    // Collect 8 beats of read data
                    if (phy_rvalid) begin
                        tile_buf[beat_cnt*64 +: 64] <= phy_rdata;
                        if (beat_cnt == 3'd7) begin
                            tile_rdata <= tile_buf;
                            tile_rdata[7*64 +: 64] <= phy_rdata;  // last beat
                            tile_ack   <= 1'b1;
                            state      <= S_IDLE;
                        end else begin
                            beat_cnt <= beat_cnt + 1;
                        end
                    end
                end

                // --------------------------------------------------------
                S_TILE_WRITE: begin
                    // Send 8 beats of write data
                    if (phy_ready) begin
                        phy_req   <= 1'b1;
                        phy_addr  <= tile_addr[31:0] + {29'd0, beat_cnt, 3'd0};
                        phy_wen   <= 1'b1;
                        phy_wdata <= tile_buf[beat_cnt*64 +: 64];
                        phy_burst_len <= (beat_cnt == 3'd0) ? 4'd8 : 4'd0;
                        if (beat_cnt == 3'd7) begin
                            tile_ack <= 1'b1;
                            state    <= S_IDLE;
                        end else begin
                            beat_cnt <= beat_cnt + 1;
                        end
                    end
                end
            endcase
        end
    end

endmodule
