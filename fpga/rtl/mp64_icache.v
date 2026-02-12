// ============================================================================
// mp64_icache.v — Megapad-64 Per-Core Instruction Cache
// ============================================================================
//
// 4 KiB direct-mapped instruction cache with 16-byte lines (256 entries).
// Designed for the Megapad-64 pipelined CPU to eliminate the byte-at-a-time
// instruction fetch bottleneck.
//
// Key design choices:
//   - Per-core (one I-cache per CPU, no coherence needed)
//   - Read-only from CPU perspective (stores go through data path)
//   - 16-byte lines: 2 bus beats to refill (64-bit bus), good spatial
//     locality for 2–3 byte average instructions
//   - Full invalidate on CSR command (self-modifying code support)
//   - Simple, deterministic — no speculative prefetch, no victim buffer
//
// Geometry (1 MiB address space, 20-bit physical):
//   - 256 lines × 16 bytes = 4 096 bytes
//   - Offset:  [3:0]   — 4 bits (byte within 16-byte line)
//   - Index:   [11:4]  — 8 bits (which of 256 lines)
//   - Tag:     [19:12] — 8 bits (disambiguate lines at same index)
//
// Interface:
//   - CPU side: address + request → hit/stall + 64-bit data (8-byte aligned)
//   - Bus side: valid/ready handshake for refill (2 × 64-bit beats)
//   - Stats:    hit/miss counters readable via CSR
//

`include "mp64_defs.vh"

module mp64_icache (
    input  wire        clk,
    input  wire        rst_n,

    // === CPU fetch interface ===
    input  wire [63:0] fetch_addr,       // byte address to fetch from
    input  wire        fetch_valid,      // CPU requesting a fetch
    output wire [63:0] fetch_data,       // 8 bytes at aligned address
    output wire        fetch_hit,        // 1 = data valid this cycle
    output wire        fetch_stall,      // 1 = miss in progress, CPU must wait

    // === Memory bus interface (for refill) ===
    output reg         bus_valid,
    output reg  [63:0] bus_addr,
    input  wire [63:0] bus_rdata,
    input  wire        bus_ready,
    output wire        bus_wen,          // always 0 (read-only cache)
    output wire [1:0]  bus_size,         // always BUS_DWORD

    // === Invalidate ===
    input  wire        inv_all,          // invalidate entire cache
    input  wire        inv_line,         // invalidate single line
    input  wire [63:0] inv_addr,         // address of line to invalidate

    // === Statistics (readable via CSR) ===
    output reg  [63:0] stat_hits,
    output reg  [63:0] stat_misses
);

    // Always read-only, always 64-bit bus reads
    assign bus_wen  = 1'b0;
    assign bus_size = BUS_DWORD;

    // ========================================================================
    // Cache geometry
    // ========================================================================
    localparam LINE_BYTES    = 16;         // 16 bytes per line
    localparam NUM_LINES     = 256;        // 256 lines = 4 KiB
    localparam OFFSET_BITS   = 4;          // log2(16)
    localparam INDEX_BITS    = 8;          // log2(256)
    localparam TAG_BITS      = 8;          // 20 - 4 - 4 - 8 (but we use [19:12])
    localparam BEATS_PER_LINE = 2;         // 16 bytes / 8 bytes per beat

    // ========================================================================
    // Cache storage
    // ========================================================================
    // Data: 256 lines × 128 bits (16 bytes per line, stored as 2 × 64-bit)
    reg [63:0] data_lo [0:NUM_LINES-1];   // bytes [7:0] of each line
    reg [63:0] data_hi [0:NUM_LINES-1];   // bytes [15:8] of each line
    reg [TAG_BITS-1:0] tags [0:NUM_LINES-1];
    reg [NUM_LINES-1:0] valid;            // packed valid bits

    // ========================================================================
    // Address decomposition
    // ========================================================================
    wire [OFFSET_BITS-1:0] addr_offset = fetch_addr[OFFSET_BITS-1:0];
    wire [INDEX_BITS-1:0]  addr_index  = fetch_addr[OFFSET_BITS+INDEX_BITS-1:OFFSET_BITS];
    wire [TAG_BITS-1:0]    addr_tag    = fetch_addr[OFFSET_BITS+INDEX_BITS+TAG_BITS-1:
                                                     OFFSET_BITS+INDEX_BITS];

    // For invalidation
    wire [INDEX_BITS-1:0]  inv_index   = inv_addr[OFFSET_BITS+INDEX_BITS-1:OFFSET_BITS];

    // ========================================================================
    // Hit detection
    // ========================================================================
    wire line_valid = valid[addr_index];
    wire tag_match = line_valid && (tags[addr_index] == addr_tag);

    // Data output: select upper or lower 64-bit half based on offset[3]
    assign fetch_data = addr_offset[3] ? data_hi[addr_index]
                                       : data_lo[addr_index];

    // ========================================================================
    // Refill FSM
    // ========================================================================
    localparam IDLE   = 2'd0;
    localparam REFILL = 2'd1;    // fetching beats from bus

    reg [1:0]              state;
    reg                    beat_count;     // 0 or 1 (2 beats per line)
    reg [INDEX_BITS-1:0]   refill_index;
    reg [TAG_BITS-1:0]     refill_tag;
    reg [63:0]             refill_base;    // line-aligned base address
    reg                    refill_pending; // 1 = waiting for bus_ready on current beat

    // ========================================================================
    // Output control
    // ========================================================================
    assign fetch_hit   = fetch_valid && tag_match && (state == IDLE);
    assign fetch_stall = fetch_valid && !fetch_hit;

    // ========================================================================
    // Main logic
    // ========================================================================
    integer i;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state      <= IDLE;
            beat_count <= 1'b0;
            bus_valid  <= 1'b0;
            stat_hits  <= 64'd0;
            stat_misses <= 64'd0;
            refill_index <= {INDEX_BITS{1'b0}};
            refill_tag   <= {TAG_BITS{1'b0}};
            refill_base  <= 64'd0;
            refill_pending <= 1'b0;

            // Invalidate all lines
            valid <= {NUM_LINES{1'b0}};
            for (i = 0; i < NUM_LINES; i = i + 1)
                tags[i]  <= {TAG_BITS{1'b0}};

        end else begin
            bus_valid <= 1'b0;  // default: no bus request

            // ----------------------------------------------------------------
            // Full cache invalidate
            // ----------------------------------------------------------------
            if (inv_all) begin
                valid <= {NUM_LINES{1'b0}};
                state <= IDLE;
            end else if (inv_line) begin
                valid <= valid & ~({{(NUM_LINES-1){1'b0}}, 1'b1} << inv_index);
            end

            // ----------------------------------------------------------------
            // FSM
            // ----------------------------------------------------------------
            case (state)
                IDLE: begin
                    if (fetch_valid && tag_match) begin
                        stat_hits <= stat_hits + 64'd1;
                    end else if (fetch_valid && !tag_match) begin
                        // Miss — begin refill
                        stat_misses <= stat_misses + 64'd1;
                        state        <= REFILL;
                        beat_count   <= 1'b0;
                        refill_pending <= 1'b1;
                        refill_index <= addr_index;
                        refill_tag   <= addr_tag;
                        refill_base  <= {fetch_addr[63:OFFSET_BITS],
                                         {OFFSET_BITS{1'b0}}};
                        // Issue first bus read immediately
                        bus_valid <= 1'b1;
                        bus_addr  <= {fetch_addr[63:OFFSET_BITS],
                                      {OFFSET_BITS{1'b0}}};
                    end
                end

                REFILL: begin
                    if (refill_pending) begin
                        // Outstanding request — keep bus_valid asserted, wait for bus_ready
                        bus_valid <= 1'b1;
                        bus_addr  <= refill_base + {60'd0, beat_count, 3'b000};

                        if (bus_ready) begin
                            // Response received for current beat
                            refill_pending <= 1'b0;
                            bus_valid      <= 1'b0;

                            if (beat_count == 1'b0) begin
                                data_lo[refill_index] <= bus_rdata;
                                beat_count <= 1'b1;
                            end else begin
                                data_hi[refill_index] <= bus_rdata;
                                tags[refill_index]    <= refill_tag;
                                valid <= valid | ({{(NUM_LINES-1){1'b0}}, 1'b1} << refill_index);
                                state <= IDLE;
                            end
                        end
                    end else begin
                        // Issue request for next beat (bus_ready from previous beat
                        // may still be high — refill_pending=0 prevents consuming it)
                        bus_valid <= 1'b1;
                        bus_addr  <= refill_base + {60'd0, beat_count, 3'b000};
                        refill_pending <= 1'b1;
                    end
                end

                default: state <= IDLE;
            endcase
        end
    end

endmodule
