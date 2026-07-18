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
    input  wire [1:0]   cpu_size,
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
    localparam [2:0] EXT_IDLE          = 3'd0;
    localparam [2:0] EXT_CPU_WAIT      = 3'd1;
    localparam [2:0] EXT_CPU_RMW_READ  = 3'd2;
    localparam [2:0] EXT_CPU_RMW_GAP   = 3'd3;
    localparam [2:0] EXT_CPU_RMW_WRITE = 3'd4;
    localparam [2:0] EXT_TILE_BURST    = 3'd5;
    localparam [2:0] EXT_CPU_CANCEL    = 3'd6;

    reg [2:0]  state;
    reg [2:0]  beat_cnt;        // 0–7 for tile burst
    reg        tile_wen_r;      // registered write direction
    reg [31:0] tile_addr_r;     // registered tile address
    reg        cpu_req_seen;
    reg [31:0] cpu_addr_r;
    reg [63:0] cpu_wdata_r;
    reg [1:0]  cpu_size_r;

    function [63:0] merge_subword;
        input [63:0] old_word;
        input [63:0] new_data;
        input [1:0]  access_size;
        input [2:0]  address_lane;
        reg [63:0] merged;
        begin
            merged = old_word;
            case (access_size)
                BUS_BYTE: merged[address_lane*8 +: 8] = new_data[7:0];
                BUS_HALF: merged[address_lane[2:1]*16 +: 16] = new_data[15:0];
                BUS_WORD: merged[address_lane[2]*32 +: 32] = new_data[31:0];
                default:  merged = new_data;
            endcase
            merge_subword = merged;
        end
    endfunction

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
            cpu_req_seen  <= 1'b0;
            cpu_addr_r    <= 32'd0;
            cpu_wdata_r   <= 64'd0;
            cpu_size_r    <= BUS_DWORD;
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

            // Upstream holds REQ until it observes registered ACK.  Remember
            // acceptance until REQ drops so the same transfer cannot re-enter
            // IDLE and execute twice.
            if (!cpu_req)
                cpu_req_seen <= 1'b0;

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
                    end else if (cpu_req && !cpu_req_seen) begin
                        // PHY transfers are always aligned 64-bit words.  A
                        // subword write first reads the containing word so all
                        // neighboring bytes can be preserved.
                        cpu_req_seen  <= 1'b1;
                        cpu_addr_r    <= cpu_addr;
                        cpu_wdata_r   <= cpu_wdata;
                        cpu_size_r    <= cpu_size;
                        phy_req       <= 1'b1;
                        phy_addr      <= {cpu_addr[31:3], 3'b000};
                        phy_burst_len <= 4'd1;
                        if (cpu_wen && (cpu_size != BUS_DWORD)) begin
                            phy_wen   <= 1'b0;
                            phy_wdata <= 64'd0;
                            state     <= EXT_CPU_RMW_READ;
                        end else begin
                            phy_wen   <= cpu_wen;
                            phy_wdata <= cpu_wdata;
                            state     <= EXT_CPU_WAIT;
                        end
                    end
                end

                // ============================================================
                // CPU_WAIT — single-beat, wait for PHY ack
                // ============================================================
                EXT_CPU_WAIT: begin
                    if (!cpu_req) begin
                        phy_req <= 1'b0;
                        state   <= EXT_CPU_CANCEL;
                    end else if (phy_ack) begin
                        cpu_rdata <= phy_rdata;
                        cpu_ack   <= 1'b1;
                        phy_req   <= 1'b0;
                        state     <= EXT_IDLE;
                    end
                end

                // ============================================================
                // CPU_RMW_READ/GAP/WRITE — preserve untouched PHY byte lanes
                // ============================================================
                EXT_CPU_RMW_READ: begin
                    if (!cpu_req) begin
                        phy_req <= 1'b0;
                        state   <= EXT_CPU_CANCEL;
                    end else if (phy_ack) begin
                        phy_req   <= 1'b0;
                        phy_wen   <= 1'b1;
                        phy_wdata <= merge_subword(phy_rdata, cpu_wdata_r,
                                                   cpu_size_r, cpu_addr_r[2:0]);
                        state     <= EXT_CPU_RMW_GAP;
                    end
                end

                EXT_CPU_RMW_GAP: begin
                    // Make the read and write distinct PHY transactions.
                    if (!cpu_req) begin
                        phy_req <= 1'b0;
                        state   <= EXT_CPU_CANCEL;
                    end else begin
                        phy_req <= 1'b1;
                        state   <= EXT_CPU_RMW_WRITE;
                    end
                end

                EXT_CPU_RMW_WRITE: begin
                    if (!cpu_req) begin
                        phy_req <= 1'b0;
                        state   <= EXT_CPU_CANCEL;
                    end else if (phy_ack) begin
                        cpu_ack <= 1'b1;
                        phy_req <= 1'b0;
                        state   <= EXT_IDLE;
                    end
                end

                EXT_CPU_CANCEL: begin
                    phy_req <= 1'b0;
                    if (!phy_ack)
                        state <= EXT_IDLE;
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
