// ============================================================================
// mp64_icache.v — Megapad-64 Per-Core Instruction Cache
// ============================================================================
//
// 4 KiB direct-mapped, read-only instruction cache.
//
// Geometry (parameterisable):
//   - 256 lines × 16 bytes = 4 096 bytes
//   - Offset:  [3:0]   — byte within 16-byte line
//   - Index:   [11:4]  — which of 256 lines
//   - Tag:     [19:12] — disambiguate aliased lines
//
// Key design choices:
//   - Per-core, no coherence protocol needed
//   - 16-byte lines → 2 bus beats (64-bit bus) to refill
//   - Full/single-line invalidation via CSR command
//   - Statistics counters for hit/miss
//   - Uses mp64_sram_sp primitives for data storage (portable)
//
// Interface:
//   CPU side:  addr + request → hit/stall + 64-bit data
//   Bus side:  valid/ready handshake for 2-beat refill
//
// Coding rules:
//   - Verilog-2001, synchronous reset, non-blocking assigns
//   - No vendor primitives (uses portable mp64_sram_sp)
//   - No `%` or `/` operators
//

`include "mp64_pkg.vh"

module mp64_icache #(
    parameter NUM_LINES    = 256,    // number of cache lines
    parameter LINE_BYTES   = 16,     // bytes per line
    parameter TAG_BITS     = 8,      // tag width
    parameter INDEX_BITS   = 8,      // log2(NUM_LINES)
    parameter OFFSET_BITS  = 4       // log2(LINE_BYTES)
) (
    input  wire        clk,
    input  wire        rst,

    // === CPU fetch interface ===
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
    output wire        bus_wen,
    output wire [1:0]  bus_size,

    // === Invalidation ===
    input  wire        inv_all,
    input  wire        inv_line,
    input  wire [63:0] inv_addr,

    // === Statistics ===
    output reg  [63:0] stat_hits,
    output reg  [63:0] stat_misses
);

    // Always read-only, always 64-bit
    assign bus_wen  = 1'b0;
    assign bus_size = BUS_DWORD;

    // ====================================================================
    // Address decomposition
    // ====================================================================
    wire [OFFSET_BITS-1:0] addr_offset = fetch_addr[OFFSET_BITS-1:0];
    wire [INDEX_BITS-1:0]  addr_index  = fetch_addr[OFFSET_BITS+INDEX_BITS-1:OFFSET_BITS];
    wire [TAG_BITS-1:0]    addr_tag    = fetch_addr[OFFSET_BITS+INDEX_BITS+TAG_BITS-1:
                                                     OFFSET_BITS+INDEX_BITS];
    wire [INDEX_BITS-1:0]  inv_index   = inv_addr[OFFSET_BITS+INDEX_BITS-1:OFFSET_BITS];

    // ====================================================================
    // Storage arrays (registered)
    // ====================================================================
    // Data: 2 × 64-bit halves per line  (lo = bytes[7:0], hi = bytes[15:8])
    // Using plain reg arrays — portable across all targets.
    // (mp64_sram_sp primitives could be used but 4 KiB is small enough
    //  for distributed RAM / LUT-RAM on most FPGAs.)
    reg [63:0] data_lo [0:NUM_LINES-1];
    reg [63:0] data_hi [0:NUM_LINES-1];
    reg [TAG_BITS-1:0] tags [0:NUM_LINES-1];
    reg [NUM_LINES-1:0] valid;

    // ====================================================================
    // Hit detection
    // ====================================================================
    wire line_valid = valid[addr_index];
    wire tag_match  = line_valid && (tags[addr_index] == addr_tag);

    // Data mux: upper or lower 64 bits based on offset[3]
    assign fetch_data = addr_offset[3] ? data_hi[addr_index]
                                       : data_lo[addr_index];

    // ====================================================================
    // Refill FSM
    // ====================================================================
    localparam IC_IDLE   = 2'd0;
    localparam IC_REFILL = 2'd1;

    reg [1:0]            state;
    reg                  beat_count;       // 0 or 1 (2 beats total)
    reg [INDEX_BITS-1:0] refill_index;
    reg [TAG_BITS-1:0]   refill_tag;
    reg [63:0]           refill_base;      // line-aligned base address
    reg                  refill_pending;

    // ====================================================================
    // Output control
    // ====================================================================
    assign fetch_hit   = fetch_valid && tag_match && (state == IC_IDLE);
    assign fetch_stall = fetch_valid && !fetch_hit;

    // ====================================================================
    // Main logic
    // ====================================================================
    integer i;

    always @(posedge clk) begin
        if (rst) begin
            state          <= IC_IDLE;
            beat_count     <= 1'b0;
            bus_valid      <= 1'b0;
            stat_hits      <= 64'd0;
            stat_misses    <= 64'd0;
            refill_index   <= {INDEX_BITS{1'b0}};
            refill_tag     <= {TAG_BITS{1'b0}};
            refill_base    <= 64'd0;
            refill_pending <= 1'b0;
            valid          <= {NUM_LINES{1'b0}};
            for (i = 0; i < NUM_LINES; i = i + 1)
                tags[i] <= {TAG_BITS{1'b0}};
        end else begin
            bus_valid <= 1'b0;

            // ==============================================================
            // Invalidation (takes priority over FSM actions on 'valid')
            // ==============================================================
            if (inv_all) begin
                valid <= {NUM_LINES{1'b0}};
                state <= IC_IDLE;
            end else if (inv_line) begin
                valid[inv_index] <= 1'b0;
            end

            // ==============================================================
            // FSM
            // ==============================================================
            case (state)
                IC_IDLE: begin
                    if (fetch_valid && tag_match) begin
                        stat_hits <= stat_hits + 64'd1;
                    end else if (fetch_valid && !tag_match) begin
                        // Cache miss — begin 2-beat refill
                        stat_misses    <= stat_misses + 64'd1;
                        state          <= IC_REFILL;
                        beat_count     <= 1'b0;
                        refill_pending <= 1'b1;
                        refill_index   <= addr_index;
                        refill_tag     <= addr_tag;
                        refill_base    <= {fetch_addr[63:OFFSET_BITS],
                                           {OFFSET_BITS{1'b0}}};
                        // Issue first bus read
                        bus_valid <= 1'b1;
                        bus_addr  <= {fetch_addr[63:OFFSET_BITS],
                                      {OFFSET_BITS{1'b0}}};
                    end
                end

                IC_REFILL: begin
                    if (refill_pending) begin
                        bus_valid <= 1'b1;
                        bus_addr  <= refill_base + {60'd0, beat_count, 3'b000};

                        if (bus_ready) begin
                            refill_pending <= 1'b0;
                            bus_valid      <= 1'b0;

                            if (beat_count == 1'b0) begin
                                data_lo[refill_index] <= bus_rdata;
                                beat_count <= 1'b1;
                            end else begin
                                data_hi[refill_index] <= bus_rdata;
                                tags[refill_index]    <= refill_tag;
                                valid[refill_index]   <= 1'b1;
                                state <= IC_IDLE;
                            end
                        end
                    end else begin
                        // Issue request for next beat
                        bus_valid      <= 1'b1;
                        bus_addr       <= refill_base + {60'd0, beat_count, 3'b000};
                        refill_pending <= 1'b1;
                    end
                end

                default: state <= IC_IDLE;
            endcase
        end
    end

endmodule
