// ============================================================================
// mp64_extmem.v — External Memory Controller (Portable)
// ============================================================================
//
// Arbitrates CPU 64-bit accesses and tile 512-bit accesses onto a decoupled
// generic PHY.  Every physical word is an independent transaction:
//
//   1. PHY_REQ and its payload remain stable until PHY_READY accepts them.
//   2. PHY_REQ drops after launch.
//   3. PHY_ACK later terminates that word; PHY_ERROR qualifies the response.
//
// A tile access is serialized into eight little-endian 64-bit words.  The
// controller retains the 512-bit write payload and read staging internally
// after TILE_ACCEPT, so the shared tile arbiter can release its payload slot.
// Both launch and response phases are bounded to 255 cycles per physical word.
// An accepted-word response timeout retires upstream at that deadline, then
// holds PHY_CANCEL until PHY_CANCEL_DONE guarantees that no response from the
// abandoned PHY epoch can arrive.  Reset uses the same flush barrier.
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

    // === Tile port (8 physical words, 512-bit) ===
    input  wire         tile_req,
    input  wire [31:0]  tile_addr,
    input  wire [511:0] tile_wdata,
    input  wire         tile_wen,
    input  wire         tile_cancel,
    output reg          tile_accept,
    output reg  [511:0] tile_rdata,
    output reg          tile_ack,
    output reg          tile_error,
    output reg  [63:0]  tile_fault_addr,
    // One pulse per successfully acknowledged physical word in the active
    // tile transaction. This sits below the 512-bit terminal boundary so a
    // faulted transfer's acknowledged prefix remains exactly accountable.
    output reg          tile_word_done,

    // === Generic PHY interface ===
    output wire         phy_req,
    output reg  [31:0]  phy_addr,
    output reg  [63:0]  phy_wdata,
    output reg          phy_wen,
    input  wire         phy_ready,
    input  wire [63:0]  phy_rdata,
    input  wire         phy_ack,
    input  wire         phy_error,
    output reg          phy_cancel,
    input  wire         phy_cancel_done,
    output reg  [3:0]   phy_burst_len
);

    `include "mp64_pkg.vh"

    // ========================================================================
    // FSM and retained transaction state
    // ========================================================================
    localparam [2:0] EXT_IDLE        = 3'd0;
    localparam [2:0] EXT_CPU_LAUNCH  = 3'd1;
    localparam [2:0] EXT_CPU_WAIT    = 3'd2;
    localparam [2:0] EXT_TILE_LAUNCH = 3'd3;
    localparam [2:0] EXT_TILE_WAIT   = 3'd4;
    localparam [2:0] EXT_PHY_FLUSH   = 3'd5;

    reg [2:0] state;
    // Counts cycles already spent in the current launch/response phase.
    // With zero loaded on entry, old value 254 is the 255th sampled cycle.
    reg [7:0] word_timer;
    reg       phy_launch_pending;

    reg        cpu_req_seen;
    reg        cpu_cancelled;
    reg [31:0] cpu_addr_r;
    reg [63:0] cpu_wdata_r;
    reg [1:0]  cpu_size_r;
    reg        cpu_rmw;
    reg        cpu_rmw_write;

    reg        tile_req_seen;
    reg        tile_cancelled;
    reg [2:0]  tile_word_index;
    reg        tile_wen_r;
    reg [31:0] tile_addr_r;
    reg [511:0] tile_wdata_r;

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
                BUS_HALF: merged[address_lane[2:1]*16 +: 16] =
                          new_data[15:0];
                BUS_WORD: merged[address_lane[2]*32 +: 32] =
                          new_data[31:0];
                default:  merged = new_data;
            endcase
            merge_subword = merged;
        end
    endfunction

    function [63:0] tile_word;
        input [511:0] image;
        input [2:0]   word_index;
        begin
            tile_word = image[word_index*64 +: 64];
        end
    endfunction

    // A stale terminal ACK is not allowed to overlap a new launch.  Keeping
    // the pending bit set means the held request appears immediately when ACK
    // returns low, without inserting an otherwise unnecessary idle cycle.
    assign phy_req = phy_launch_pending && !phy_ack;

    // ========================================================================
    // Main FSM
    // ========================================================================
    always @(posedge clk) begin
        if (!rst_n) begin
            // Synchronous reset invalidates any response epoch that may have
            // launched on or before this edge.  Do not reopen the upstream
            // boundary until the PHY explicitly completes the flush.
            state            <= EXT_PHY_FLUSH;
            word_timer       <= 8'd0;
            phy_launch_pending <= 1'b0;
            cpu_req_seen     <= 1'b0;
            cpu_cancelled    <= 1'b0;
            cpu_addr_r       <= 32'd0;
            cpu_wdata_r      <= 64'd0;
            cpu_size_r       <= BUS_DWORD;
            cpu_rmw          <= 1'b0;
            cpu_rmw_write    <= 1'b0;
            tile_req_seen    <= 1'b0;
            tile_cancelled   <= 1'b0;
            tile_word_index  <= 3'd0;
            tile_wen_r       <= 1'b0;
            tile_addr_r      <= 32'd0;
            tile_wdata_r     <= 512'd0;
            cpu_rdata        <= 64'd0;
            cpu_ack          <= 1'b0;
            tile_accept      <= 1'b0;
            tile_rdata       <= 512'd0;
            tile_ack         <= 1'b0;
            tile_error       <= 1'b0;
            tile_fault_addr  <= 64'd0;
            tile_word_done   <= 1'b0;
            phy_addr         <= 32'd0;
            phy_wdata        <= 64'd0;
            phy_wen          <= 1'b0;
            phy_cancel       <= 1'b1;
            phy_burst_len    <= 4'd0;
        end else begin
            cpu_ack         <= 1'b0;
            tile_accept     <= 1'b0;
            tile_ack        <= 1'b0;
            tile_error      <= 1'b0;
            tile_fault_addr <= 64'd0;
            tile_word_done  <= 1'b0;

            // A held upstream request is accepted only once.  Tile payloads
            // may be released immediately after the explicit accept pulse.
            if (!cpu_req)
                cpu_req_seen <= 1'b0;
            if (!tile_req)
                tile_req_seen <= 1'b0;

            case (state)
                // ============================================================
                // IDLE — capture one complete upstream payload; tile priority
                // ============================================================
                EXT_IDLE: begin
                    phy_launch_pending <= 1'b0;
                    phy_cancel <= 1'b0;

                    if (tile_req && !tile_req_seen) begin
                        tile_req_seen   <= 1'b1;
                        tile_accept     <= 1'b1;
                        tile_cancelled  <= tile_cancel;
                        tile_word_index <= 3'd0;
                        tile_wen_r      <= tile_wen;
                        tile_addr_r     <= tile_addr;
                        tile_wdata_r    <= tile_wdata;
                        phy_addr        <= tile_addr;
                        phy_wdata       <= tile_wdata[63:0];
                        phy_wen         <= tile_wen;
                        phy_burst_len   <= 4'd1;
                        phy_launch_pending <= 1'b1;
                        word_timer      <= 8'd0;
                        state           <= EXT_TILE_LAUNCH;
                    end else if (cpu_req && !cpu_req_seen) begin
                        cpu_req_seen  <= 1'b1;
                        cpu_cancelled <= 1'b0;
                        cpu_addr_r    <= cpu_addr;
                        cpu_wdata_r   <= cpu_wdata;
                        cpu_size_r    <= cpu_size;
                        cpu_rmw       <= cpu_wen &&
                                         (cpu_size != BUS_DWORD);
                        cpu_rmw_write <= 1'b0;
                        phy_addr      <= {cpu_addr[31:3], 3'b000};
                        phy_burst_len <= 4'd1;
                        if (cpu_wen && (cpu_size != BUS_DWORD)) begin
                            phy_wen   <= 1'b0;
                            phy_wdata <= 64'd0;
                        end else begin
                            phy_wen   <= cpu_wen;
                            phy_wdata <= cpu_wdata;
                        end
                        phy_launch_pending <= 1'b1;
                        word_timer <= 8'd0;
                        state      <= EXT_CPU_LAUNCH;
                    end
                end

                // ============================================================
                // CPU_LAUNCH — hold request until ready, bounded to 255 cycles
                // ============================================================
                EXT_CPU_LAUNCH: begin
                    if (phy_req && phy_ready) begin
                        // A simultaneous withdrawal cannot retract a launch
                        // already observed by the PHY; drain it instead.
                        phy_launch_pending <= 1'b0;
                        if (!cpu_req)
                            cpu_cancelled <= 1'b1;
                        word_timer <= 8'd0;
                        state      <= EXT_CPU_WAIT;
                    end else if (!cpu_req) begin
                        // No PHY launch occurred, so cancellation is immediate.
                        phy_launch_pending <= 1'b0;
                        cpu_cancelled <= 1'b1;
                        state         <= EXT_IDLE;
                    end else if (word_timer == 8'd254) begin
                        // CPU has no architectural error sideband yet.  Return
                        // a terminal ACK with zero data rather than deadlock.
                        phy_launch_pending <= 1'b0;
                        cpu_rdata  <= 64'd0;
                        cpu_ack    <= 1'b1;
                        state      <= EXT_IDLE;
                    end else begin
                        word_timer <= word_timer + 8'd1;
                    end
                end

                // ============================================================
                // CPU_WAIT — drain a launched word through ACK or timeout
                // ============================================================
                EXT_CPU_WAIT: begin
                    if (!cpu_req)
                        cpu_cancelled <= 1'b1;

                    if (phy_ack) begin
                        if (cpu_cancelled || !cpu_req) begin
                            // A withdrawn request cannot consume the response.
                            state <= EXT_IDLE;
                        end else if (phy_error) begin
                            // See the CPU error-sideband seam above.
                            cpu_rdata <= 64'd0;
                            cpu_ack   <= 1'b1;
                            state     <= EXT_IDLE;
                        end else if (cpu_rmw && !cpu_rmw_write) begin
                            // The subword read completed.  Launch a distinct
                            // full-word write only after ACK returns low.
                            phy_wen       <= 1'b1;
                            phy_wdata     <= merge_subword(
                                phy_rdata, cpu_wdata_r, cpu_size_r,
                                cpu_addr_r[2:0]);
                            cpu_rmw_write <= 1'b1;
                            phy_launch_pending <= 1'b1;
                            word_timer    <= 8'd0;
                            state         <= EXT_CPU_LAUNCH;
                        end else begin
                            cpu_rdata <= phy_rdata;
                            cpu_ack   <= 1'b1;
                            state     <= EXT_IDLE;
                        end
                    end else if (word_timer == 8'd254) begin
                        phy_launch_pending <= 1'b0;
                        if (!(cpu_cancelled || !cpu_req)) begin
                            cpu_rdata <= 64'd0;
                            cpu_ack   <= 1'b1;
                        end
                        // Upstream retires at the architectural deadline, but
                        // the controller remains closed until the PHY proves
                        // the abandoned response can no longer arrive.
                        phy_cancel <= 1'b1;
                        state      <= EXT_PHY_FLUSH;
                    end else begin
                        word_timer <= word_timer + 8'd1;
                    end
                end

                // ============================================================
                // TILE_LAUNCH — each of eight words is a separate PHY request
                // ============================================================
                EXT_TILE_LAUNCH: begin
                    if (phy_req && phy_ready) begin
                        // READY on the 255th launch cycle wins.  Cancellation
                        // sampled with that launch is retained for drain.
                        phy_launch_pending <= 1'b0;
                        if (tile_cancel)
                            tile_cancelled <= 1'b1;
                        word_timer <= 8'd0;
                        state      <= EXT_TILE_WAIT;
                    end else if (tile_cancelled || tile_cancel) begin
                        // Cancellation before this word launches is complete:
                        // no response needs draining and no later word issues.
                        tile_cancelled <= 1'b1;
                        phy_launch_pending <= 1'b0;
                        tile_ack       <= 1'b1;
                        state          <= EXT_IDLE;
                    end else if (word_timer == 8'd254) begin
                        phy_launch_pending <= 1'b0;
                        tile_ack        <= 1'b1;
                        tile_error      <= 1'b1;
                        tile_fault_addr <= {32'd0, phy_addr};
                        state           <= EXT_IDLE;
                    end else begin
                        word_timer <= word_timer + 8'd1;
                    end
                end

                // ============================================================
                // TILE_WAIT — finish current word, then prepare the next one
                // ============================================================
                EXT_TILE_WAIT: begin
                    if (tile_cancel)
                        tile_cancelled <= 1'b1;

                    if (phy_ack) begin
                        if (tile_cancelled || tile_cancel) begin
                            // The accepted word has been drained.  Its write
                            // may be visible, but no additional word launches.
                            tile_ack <= 1'b1;
                            state    <= EXT_IDLE;
                        end else if (phy_error) begin
                            tile_ack        <= 1'b1;
                            tile_error      <= 1'b1;
                            tile_fault_addr <= {32'd0, phy_addr};
                            state           <= EXT_IDLE;
                        end else begin
                            tile_word_done <= 1'b1;
                            if (!tile_wen_r)
                                tile_rdata[tile_word_index*64 +: 64] <=
                                    phy_rdata;

                            if (tile_word_index == 3'd7) begin
                                tile_ack <= 1'b1;
                                state    <= EXT_IDLE;
                            end else begin
                                tile_word_index <= tile_word_index + 3'd1;
                                phy_addr <= tile_addr_r +
                                    {26'd0, tile_word_index + 3'd1, 3'b000};
                                phy_wdata <= tile_word(
                                    tile_wdata_r,
                                    tile_word_index + 3'd1);
                                phy_wen      <= tile_wen_r;
                                phy_launch_pending <= 1'b1;
                                word_timer   <= 8'd0;
                                state        <= EXT_TILE_LAUNCH;
                            end
                        end
                    end else if (word_timer == 8'd254) begin
                        phy_launch_pending <= 1'b0;
                        tile_ack <= 1'b1;
                        if (!(tile_cancelled || tile_cancel)) begin
                            tile_error      <= 1'b1;
                            tile_fault_addr <= {32'd0, phy_addr};
                        end
                        phy_cancel <= 1'b1;
                        state      <= EXT_PHY_FLUSH;
                    end else begin
                        word_timer <= word_timer + 8'd1;
                    end
                end

                // ============================================================
                // PHY_FLUSH — suppress stale response epoch before reuse
                // ============================================================
                EXT_PHY_FLUSH: begin
                    phy_launch_pending <= 1'b0;
                    phy_cancel         <= 1'b1;
                    // PHY_CANCEL_DONE is a level handshake: while CANCEL is
                    // held, DONE must remain asserted until this edge.  DONE
                    // guarantees PHY_ACK is low and no canceled response can
                    // be emitted later.
                    if (phy_cancel_done) begin
                        phy_cancel <= 1'b0;
                        state      <= EXT_IDLE;
                    end
                end

                default: begin
                    phy_launch_pending <= 1'b0;
                    phy_cancel         <= 1'b1;
                    state              <= EXT_PHY_FLUSH;
                end
            endcase
        end
    end

endmodule
