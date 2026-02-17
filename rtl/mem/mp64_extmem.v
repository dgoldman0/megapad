// ============================================================================
// mp64_extmem.v — External Memory Controller (Portable)
// ============================================================================
//
// Arbitrates between CPU single-beat and tile 8-beat burst accesses
// onto a generic PHY interface.
//
// Tile requests pre-empt CPU requests (tile has strict priority).
//
// PHY interface is generic: adaptable to HyperRAM, SDRAM, QPI flash.
//   - Single phy_req / phy_ack handshake per beat
//   - phy_burst_len signals burst length (1 for CPU, 8 for tile)
//
// Tile burst: 8 beats × 64 bits = 512 bits.
//   Read:  latch 8 sequential phy_rdata into tile_rdata shift register.
//   Write: shift out 8 sequential 64-bit words from tile_wdata.
//
// Coding standard: Verilog-2001, sync reset, non-blocking assigns.
//

module mp64_extmem (
    input  wire         clk,
    input  wire         rst_n,

    // === CPU port (single-beat, 64-bit) ===
    input  wire         cpu_req,
    input  wire [31:0]  cpu_addr,
    input  wire [63:0]  cpu_wdata,
    input  wire         cpu_wen,
    output reg  [63:0]  cpu_rdata,
    output reg          cpu_ack,

    // === Tile port (8-beat burst, 512-bit) ===
    input  wire         tile_req,
    input  wire [31:0]  tile_addr,
    input  wire [511:0] tile_wdata,
    input  wire         tile_wen,
    output reg  [511:0] tile_rdata,
    output reg          tile_ack,

    // === Generic PHY interface ===
    output reg          phy_req,
    output reg  [31:0]  phy_addr,
    output reg  [63:0]  phy_wdata,
    output reg          phy_wen,
    input  wire [63:0]  phy_rdata,
    input  wire         phy_ack,
    output reg  [3:0]   phy_burst_len
);

    `include "mp64_pkg.vh"

    // ========================================================================
    // FSM states
    // ========================================================================
    localparam [2:0] EXT_IDLE       = 3'd0;
    localparam [2:0] EXT_CPU_WAIT   = 3'd1;
    localparam [2:0] EXT_TILE_BURST = 3'd2;

    reg [2:0]  state;
    reg [2:0]  beat_cnt;        // 0–7 for tile burst
    reg        tile_wen_r;      // registered write direction
    reg [31:0] tile_addr_r;     // registered tile address

    // ========================================================================
    // Tile wdata / rdata as 8 × 64-bit words
    // ========================================================================
    wire [63:0] tile_word [0:7];
    genvar gi;
    generate
        for (gi = 0; gi < 8; gi = gi + 1) begin : g_tile_word
            assign tile_word[gi] = tile_wdata[gi*64 +: 64];
        end
    endgenerate

    // ========================================================================
    // Main FSM
    // ========================================================================
    always @(posedge clk) begin
        if (!rst_n) begin
            state         <= EXT_IDLE;
            beat_cnt      <= 3'd0;
            tile_wen_r    <= 1'b0;
            tile_addr_r   <= 32'd0;
            cpu_ack       <= 1'b0;
            cpu_rdata     <= 64'd0;
            tile_ack      <= 1'b0;
            tile_rdata    <= 512'd0;
            phy_req       <= 1'b0;
            phy_addr      <= 32'd0;
            phy_wdata     <= 64'd0;
            phy_wen       <= 1'b0;
            phy_burst_len <= 4'd0;
        end else begin
            cpu_ack  <= 1'b0;
            tile_ack <= 1'b0;

            case (state)

                // ============================================================
                // IDLE — tile has priority
                // ============================================================
                EXT_IDLE: begin
                    if (tile_req) begin
                        // Start 8-beat burst
                        phy_req       <= 1'b1;
                        phy_addr      <= tile_addr;
                        phy_wen       <= tile_wen;
                        phy_burst_len <= 4'd8;
                        phy_wdata     <= tile_word[0];
                        tile_wen_r    <= tile_wen;
                        tile_addr_r   <= tile_addr;
                        beat_cnt      <= 3'd0;
                        state         <= EXT_TILE_BURST;
                    end else if (cpu_req) begin
                        // Start single-beat
                        phy_req       <= 1'b1;
                        phy_addr      <= cpu_addr;
                        phy_wen       <= cpu_wen;
                        phy_burst_len <= 4'd1;
                        phy_wdata     <= cpu_wdata;
                        state         <= EXT_CPU_WAIT;
                    end
                end

                // ============================================================
                // CPU_WAIT — single-beat, wait for PHY ack
                // ============================================================
                EXT_CPU_WAIT: begin
                    if (phy_ack) begin
                        cpu_rdata <= phy_rdata;
                        cpu_ack   <= 1'b1;
                        phy_req   <= 1'b0;
                        state     <= EXT_IDLE;
                    end
                end

                // ============================================================
                // TILE_BURST — 8-beat burst read or write
                // ============================================================
                EXT_TILE_BURST: begin
                    if (phy_ack) begin
                        // Capture read data (even for writes, harmless)
                        tile_rdata[beat_cnt*64 +: 64] <= phy_rdata;

                        if (beat_cnt == 3'd7) begin
                            // Last beat
                            tile_ack  <= 1'b1;
                            phy_req   <= 1'b0;
                            state     <= EXT_IDLE;
                        end else begin
                            // Advance to next beat
                            beat_cnt  <= beat_cnt + 3'd1;
                            phy_addr  <= tile_addr_r +
                                         {25'd0, (beat_cnt + 3'd1), 3'd0};
                            phy_wdata <= tile_word[beat_cnt + 3'd1];
                        end
                    end
                end

                default: begin
                    phy_req <= 1'b0;
                    state   <= EXT_IDLE;
                end

            endcase
        end
    end

endmodule
