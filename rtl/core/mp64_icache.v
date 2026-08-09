// ============================================================================
// mp64_icache.v — Megapad-64 Per-Core Instruction Cache
// ============================================================================
//
// 4 KiB direct-mapped, read-only instruction cache.
//
// Fixed architectural geometry:
//   - 256 lines × 16 bytes = 4 096 bytes
//   - Offset:  [3:0]   — byte within 16-byte line
//   - Index:   [11:4]  — which of 256 lines
//   - Tag:     full 64-bit, line-aligned physical address
//
// Key design choices:
//   - Per-core and deliberately non-coherent
//   - 16-byte lines → 2 bus beats (64-bit bus) to refill
//   - Disabled requests bypass with one aligned 64-bit bus read
//   - Full/single-line invalidation via CSR command
//   - Statistics counters for hit/miss
//
// Interface:
//   CPU side:  addr + request → hit/stall + 64-bit data
//   Bus side:  valid/ready handshake for refill or bypass
//
// Coding rules:
//   - Verilog-2001, synchronous reset, non-blocking assigns
//   - No vendor primitives (uses portable mp64_sram_sp)
//   - No `%` or `/` operators
//

`include "mp64_pkg.vh"

module mp64_icache (
    input  wire        clk,
    input  wire        rst,

    // === CPU fetch interface ===
    input  wire        enabled,
    input  wire [63:0] fetch_addr,
    input  wire        fetch_valid,
    output wire [63:0] fetch_data,
    output wire        fetch_hit,
    output wire        fetch_stall,

    // === Memory bus interface (refill) ===
    output reg         bus_valid,
    output reg  [63:0] bus_addr,
    input  wire [63:0] bus_rdata,
    input  wire        bus_ready,
    input  wire        bus_error,
    output wire        bus_wen,
    output wire [1:0]  bus_size,

    // === Invalidation ===
    input  wire        inv_all,
    input  wire        inv_line,
    input  wire [63:0] inv_addr,
    input  wire [6:0]  inv_size,

    // === Statistics ===
    output reg  [63:0] stat_hits,
    output reg  [63:0] stat_misses
);

    localparam NUM_LINES   = 256;
    localparam LINE_BYTES  = 16;
    localparam INDEX_BITS  = 8;
    localparam OFFSET_BITS = 4;

    // Always read-only, always 64-bit
    assign bus_wen  = 1'b0;
    assign bus_size = BUS_DWORD;

    // ====================================================================
    // Address decomposition
    // ====================================================================
    wire [OFFSET_BITS-1:0] addr_offset = fetch_addr[OFFSET_BITS-1:0];
    wire [INDEX_BITS-1:0]  addr_index  = fetch_addr[OFFSET_BITS+INDEX_BITS-1:OFFSET_BITS];
    wire [63:0]            addr_tag    = {fetch_addr[63:OFFSET_BITS],
                                          {OFFSET_BITS{1'b0}}};
    wire [63:0] inv_tag0 = {inv_addr[63:OFFSET_BITS],
                            {OFFSET_BITS{1'b0}}};
    wire [63:0] inv_tag1 = inv_tag0 + 64'd16;
    wire [63:0] inv_tag2 = inv_tag0 + 64'd32;
    wire [63:0] inv_tag3 = inv_tag0 + 64'd48;
    wire [63:0] inv_tag4 = inv_tag0 + 64'd64;
    // Architectural writers issue at most one 64-byte tile span.  Treat an
    // invalid larger request conservatively as a private-cache flush without
    // resetting statistics, rather than wrapping the three-bit line count.
    wire inv_oversize = inv_size > 7'd64;
    wire [6:0] inv_span_size = inv_oversize ? 7'd64 : inv_size;
    wire [7:0] inv_extent = {4'd0, inv_addr[3:0]}
                          + {1'b0, inv_span_size};
    wire [2:0] inv_line_count = (inv_size == 7'd0)
                              ? 3'd0
                              : (inv_extent + 8'd15) >> 4;
    wire [INDEX_BITS-1:0] inv_index0 =
        inv_tag0[OFFSET_BITS+INDEX_BITS-1:OFFSET_BITS];
    wire [INDEX_BITS-1:0] inv_index1 =
        inv_tag1[OFFSET_BITS+INDEX_BITS-1:OFFSET_BITS];
    wire [INDEX_BITS-1:0] inv_index2 =
        inv_tag2[OFFSET_BITS+INDEX_BITS-1:OFFSET_BITS];
    wire [INDEX_BITS-1:0] inv_index3 =
        inv_tag3[OFFSET_BITS+INDEX_BITS-1:OFFSET_BITS];
    wire [INDEX_BITS-1:0] inv_index4 =
        inv_tag4[OFFSET_BITS+INDEX_BITS-1:OFFSET_BITS];

    // ====================================================================
    // Storage arrays (registered)
    // ====================================================================
    // Data: 2 × 64-bit halves per line  (lo = bytes[7:0], hi = bytes[15:8])
    // Using plain reg arrays — portable across all targets.
    // (mp64_sram_sp primitives could be used but 4 KiB is small enough
    //  for distributed RAM / LUT-RAM on most FPGAs.)
    reg [63:0] data_lo [0:NUM_LINES-1];
    reg [63:0] data_hi [0:NUM_LINES-1];
    reg [63:0] tags [0:NUM_LINES-1];
    reg [NUM_LINES-1:0] valid;

    // ====================================================================
    // Hit detection
    // ====================================================================
    wire line_valid = valid[addr_index];
    wire tag_match  = line_valid && (tags[addr_index] == addr_tag);
    wire inv_matches_fetch = inv_line
                           && (inv_oversize
                            || (inv_line_count > 3'd0 && addr_tag == inv_tag0)
                            || (inv_line_count > 3'd1 && addr_tag == inv_tag1)
                            || (inv_line_count > 3'd2 && addr_tag == inv_tag2)
                            || (inv_line_count > 3'd3 && addr_tag == inv_tag3)
                            || (inv_line_count > 3'd4 && addr_tag == inv_tag4));
    wire cache_hit = enabled && tag_match && !inv_all && !inv_matches_fetch;

    // Data mux: upper or lower 64 bits based on offset[3]
    wire [63:0] cached_data = addr_offset[3] ? data_hi[addr_index]
                                             : data_lo[addr_index];

    // ====================================================================
    // Refill FSM
    // ====================================================================
    localparam IC_IDLE       = 3'd0;
    localparam IC_REFILL_LO  = 3'd1;
    localparam IC_REFILL_GAP = 3'd2;
    localparam IC_REFILL_HI  = 3'd3;
    localparam IC_BYPASS     = 3'd4;
    localparam IC_RESPONSE   = 3'd5;
    localparam IC_CANCEL     = 3'd6;

    reg [2:0]            state;
    reg [INDEX_BITS-1:0] refill_index;
    reg [63:0]           refill_tag;
    reg [63:0]           refill_base;      // line-aligned base address
    reg                  refill_upper;
    reg                  refill_killed;
    reg [63:0]           response_data;
    wire inv_matches_refill = inv_line
                            && (inv_oversize
                             || (inv_line_count > 3'd0
                                 && refill_tag == inv_tag0)
                             || (inv_line_count > 3'd1
                                 && refill_tag == inv_tag1)
                             || (inv_line_count > 3'd2
                                 && refill_tag == inv_tag2)
                             || (inv_line_count > 3'd3
                                 && refill_tag == inv_tag3)
                             || (inv_line_count > 3'd4
                                 && refill_tag == inv_tag4));

    // ====================================================================
    // Output control
    // ====================================================================
    assign fetch_data  = (state == IC_RESPONSE) ? response_data : cached_data;
    assign fetch_hit   = fetch_valid && !inv_all
                       && (((state == IC_IDLE) && cache_hit)
                           || (state == IC_RESPONSE));
    assign fetch_stall = fetch_valid && !fetch_hit;

    // ====================================================================
    // Main logic
    // ====================================================================
    integer i;

    always @(posedge clk) begin
        if (rst) begin
            state         <= IC_IDLE;
            bus_valid     <= 1'b0;
            bus_addr      <= 64'd0;
            stat_hits     <= 64'd0;
            stat_misses   <= 64'd0;
            refill_index  <= {INDEX_BITS{1'b0}};
            refill_tag    <= 64'd0;
            refill_base   <= 64'd0;
            refill_upper  <= 1'b0;
            refill_killed <= 1'b0;
            response_data <= 64'd0;
            valid         <= {NUM_LINES{1'b0}};
            for (i = 0; i < NUM_LINES; i = i + 1)
                tags[i] <= 64'd0;
        end else begin
            // Invalidate-all is also the architectural counter reset.  An
            // already offered bus request remains valid until READY, but its
            // result is drained rather than published.
            if (inv_all) begin
                valid         <= {NUM_LINES{1'b0}};
                stat_hits     <= 64'd0;
                stat_misses   <= 64'd0;
                refill_killed <= 1'b0;
                if (bus_valid && !bus_ready
                        && (state == IC_REFILL_LO
                         || state == IC_REFILL_HI
                         || state == IC_BYPASS
                         || state == IC_CANCEL)) begin
                    state     <= IC_CANCEL;
                    bus_valid <= 1'b1;
                end else begin
                    state     <= IC_IDLE;
                    bus_valid <= 1'b0;
                end
            end else begin
                // Span invalidation is tag-aware and covers up to five lines
                // (an unaligned 64-byte tile write).  A matching refill drains
                // accepted traffic but may not publish the line afterward.
                if (inv_line) begin
                    if (inv_oversize) begin
                        valid <= {NUM_LINES{1'b0}};
                    end else begin
                        if (inv_line_count > 3'd0
                                && valid[inv_index0]
                                && tags[inv_index0] == inv_tag0)
                            valid[inv_index0] <= 1'b0;
                        if (inv_line_count > 3'd1
                                && valid[inv_index1]
                                && tags[inv_index1] == inv_tag1)
                            valid[inv_index1] <= 1'b0;
                        if (inv_line_count > 3'd2
                                && valid[inv_index2]
                                && tags[inv_index2] == inv_tag2)
                            valid[inv_index2] <= 1'b0;
                        if (inv_line_count > 3'd3
                                && valid[inv_index3]
                                && tags[inv_index3] == inv_tag3)
                            valid[inv_index3] <= 1'b0;
                        if (inv_line_count > 3'd4
                                && valid[inv_index4]
                                && tags[inv_index4] == inv_tag4)
                            valid[inv_index4] <= 1'b0;
                    end
                    if ((state == IC_REFILL_LO
                             || state == IC_REFILL_GAP
                             || state == IC_REFILL_HI)
                            && inv_matches_refill)
                        refill_killed <= 1'b1;
                end

                case (state)
                    IC_IDLE: begin
                        bus_valid     <= 1'b0;
                        refill_killed <= 1'b0;
                        if (fetch_valid && enabled && tag_match
                                && !inv_matches_fetch) begin
                            stat_hits <= stat_hits + 64'd1;
                        end else if (fetch_valid && enabled) begin
                            // Enabled miss: fetch both aligned DWORDs.
                            stat_misses   <= stat_misses + 64'd1;
                            state         <= IC_REFILL_LO;
                            refill_index  <= addr_index;
                            refill_tag    <= addr_tag;
                            refill_base   <= addr_tag;
                            refill_upper  <= addr_offset[3];
                            bus_valid     <= 1'b1;
                            bus_addr      <= addr_tag;
                        end else if (fetch_valid) begin
                            // Disabled cache: a single uncached DWORD read,
                            // aligned to the fetch beat rather than the line.
                            state     <= IC_BYPASS;
                            bus_valid <= 1'b1;
                            bus_addr  <= {fetch_addr[63:3], 3'b000};
                        end
                    end

                    IC_REFILL_LO: begin
                        bus_valid <= 1'b1;
                        if (bus_ready) begin
                            bus_valid <= 1'b0;
                            if (bus_error) begin
                                // The owner-qualified error is consumed by
                                // the CPU as an instruction-side BUS fault.
                                // Do not cache the arbiter's sentinel or
                                // issue the second line beat.
                                valid[refill_index] <= 1'b0;
                                state <= IC_IDLE;
                            end else begin
                                data_lo[refill_index] <= bus_rdata;
                                // Insert a request-low cycle between beats.
                                // This also flushes registered READY before
                                // the second address is presented.
                                state <= IC_REFILL_GAP;
                                bus_addr <= refill_base + 64'd8;
                            end
                        end
                    end

                    IC_REFILL_GAP: begin
                        bus_valid <= 1'b1;
                        state <= IC_REFILL_HI;
                    end

                    IC_REFILL_HI: begin
                        bus_valid <= 1'b1;
                        if (bus_ready) begin
                            bus_valid <= 1'b0;
                            if (bus_error) begin
                                valid[refill_index] <= 1'b0;
                                state <= IC_IDLE;
                            end else begin
                                data_hi[refill_index] <= bus_rdata;
                                if (!refill_killed
                                        && !inv_matches_refill) begin
                                    tags[refill_index]  <= refill_tag;
                                    valid[refill_index] <= 1'b1;
                                    response_data <= refill_upper
                                                   ? bus_rdata
                                                   : data_lo[refill_index];
                                    state <= IC_RESPONSE;
                                end else begin
                                    valid[refill_index] <= 1'b0;
                                    state <= IC_IDLE;
                                end
                            end
                        end
                    end

                    IC_BYPASS: begin
                        bus_valid <= 1'b1;
                        if (bus_ready) begin
                            bus_valid <= 1'b0;
                            if (bus_error) begin
                                state <= IC_IDLE;
                            end else begin
                                response_data <= bus_rdata;
                                state <= IC_RESPONSE;
                            end
                        end
                    end

                    IC_RESPONSE: begin
                        // The requester consumes this response on this edge.
                        // A refilled miss is therefore not counted again as
                        // a cache hit.
                        bus_valid <= 1'b0;
                        state <= IC_IDLE;
                    end

                    IC_CANCEL: begin
                        bus_valid <= 1'b1;
                        if (bus_ready) begin
                            bus_valid <= 1'b0;
                            state <= IC_IDLE;
                        end
                    end

                    default: begin
                        state <= IC_IDLE;
                        bus_valid <= 1'b0;
                    end
                endcase
            end
        end
    end

endmodule
