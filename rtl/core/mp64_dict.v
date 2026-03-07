// ============================================================================
// mp64_dict.v — Forth Dictionary Search Engine (EXT.DICT, prefix FA)
// ============================================================================
//
// Tightly-coupled CPU sub-module implementing hardware-accelerated
// dictionary search via a BRAM-backed 4-way set-associative hash table.
//
// Sub-ops:
//   DFIND  (00) — lookup counted-string at Rs; Rd ← XT if found; Z=found
//   DINS   (01) — insert: Rs=name addr, Rd=XT to store; Z=success, V=overflow
//   DDEL   (02) — delete by name at Rs; Z=found-and-deleted
//   DCLR   (03) — clear entire hash table (bulk-zero valid bits)
//
// Hash table geometry:
//   - 64 sets × 4 ways = 256 entries
//   - Each entry: 1-bit valid + 32-bit hash + 5-bit name_len + 31 bytes name
//                 + 64-bit XT = 370 bits → round to 384 bits (48 bytes)
//   - BRAM layout: 4 parallel 64-set arrays (one per way)
//   - Hash function: FNV-1a 32-bit over name bytes
//
// Uses the CPU's existing bus master to read the counted-string from RAM.
// Byte-at-a-time name reading, then single-cycle BRAM lookup.
//
// Verilog-2001.  No vendor primitives.
//

module mp64_dict (
    input  wire        clk,
    input  wire        rst,

    // --- CPU handshake ---
    input  wire        start,         // pulse: begin operation
    input  wire [3:0]  op,            // sub-op (00–03)
    input  wire [63:0] name_addr,     // Rs: address of counted-string
    input  wire [63:0] xt_in,         // Rd: XT for DINS
    output reg         done,          // pulse: operation complete
    output reg  [63:0] xt_out,        // DFIND: result XT
    output reg         flag_z,        // found / success
    output reg         flag_v,        // overflow (DINS only)

    // --- Bus master (for reading name bytes from RAM) ---
    output reg         bus_req,
    output reg         bus_wr,
    output reg  [63:0] bus_addr,
    output reg  [63:0] bus_wdata,
    input  wire        bus_ack,
    input  wire [63:0] bus_rdata,

    // --- Broadcast port (DINS: out to other cores) ---
    output reg         bcast_valid,
    output reg  [31:0] bcast_hash,
    output reg  [4:0]  bcast_name_len,
    output reg  [247:0] bcast_name,   // 31 bytes
    output reg  [63:0] bcast_xt,
    input  wire        bcast_ack,

    // --- Snoop port (DINS from other cores) ---
    input  wire        snoop_valid,
    input  wire [31:0] snoop_hash,
    input  wire [4:0]  snoop_name_len,
    input  wire [247:0] snoop_name,
    input  wire [63:0] snoop_xt,
    output reg         snoop_ack
);

    // ====================================================================
    // Parameters
    // ====================================================================
    localparam NSETS    = 64;           // 2^6 sets
    localparam SET_BITS = 6;
    localparam NWAYS   = 4;
    localparam MAX_NAME = 31;          // max name length stored

    // FNV-1a 32-bit constants
    localparam [31:0] FNV_OFFSET = 32'h811C9DC5;
    localparam [31:0] FNV_PRIME  = 32'h01000193;

    // ====================================================================
    // FSM states
    // ====================================================================
    localparam [3:0] D_IDLE     = 4'd0;
    localparam [3:0] D_RD_LEN  = 4'd1;   // read counted-string length byte
    localparam [3:0] D_WAIT_LEN= 4'd2;   // wait ack for length read
    localparam [3:0] D_RD_NAME = 4'd3;   // read name byte
    localparam [3:0] D_WAIT_NM = 4'd4;   // wait ack for name byte
    localparam [3:0] D_LOOKUP  = 4'd5;   // BRAM read + compare
    localparam [3:0] D_INSERT  = 4'd6;   // BRAM write (DINS)
    localparam [3:0] D_DELETE  = 4'd7;   // BRAM invalidate (DDEL)
    localparam [3:0] D_CLEAR   = 4'd8;   // bulk-clear valid bits
    localparam [3:0] D_BCAST   = 4'd9;   // broadcast DINS to other cores
    localparam [3:0] D_DONE    = 4'd10;

    // ====================================================================
    // Hash table BRAM — 4 ways × 64 sets
    // Entry packing (384 bits = 6 × 64-bit words):
    //   [0]     = valid (1 bit)
    //   [32:1]  = hash (32 bits)
    //   [37:33] = name_len (5 bits)
    //   [285:38] = name bytes (31 × 8 = 248 bits)
    //   [349:286] = XT (64 bits)
    //
    // For simulation, use reg arrays with indexed access.
    // Synthesis would infer block RAM.
    // ====================================================================
    reg         way_valid [0:NWAYS-1][0:NSETS-1];
    reg [31:0]  way_hash  [0:NWAYS-1][0:NSETS-1];
    reg [4:0]   way_nlen  [0:NWAYS-1][0:NSETS-1];
    reg [247:0] way_name  [0:NWAYS-1][0:NSETS-1];
    reg [63:0]  way_xt    [0:NWAYS-1][0:NSETS-1];

    // ====================================================================
    // Working registers
    // ====================================================================
    reg [3:0]  state;
    reg [3:0]  cur_op;
    reg [63:0] name_ptr;         // current read address into name
    reg [4:0]  name_len;         // length from counted string
    reg [4:0]  byte_idx;         // index of next name byte to read
    reg [247:0] name_buf;        // buffered name bytes
    reg [31:0] hash_acc;         // running FNV-1a hash
    reg [SET_BITS-1:0] set_idx;  // hash[5:0] selects set
    reg [63:0] save_xt;          // latched XT for DINS

    // Combinational: match detection across 4 ways
    wire [NWAYS-1:0] match;
    wire [NWAYS-1:0] valid;
    wire [NWAYS-1:0] empty;

    genvar w;
    generate
        for (w = 0; w < NWAYS; w = w + 1) begin : way_match
            assign valid[w] = way_valid[w][set_idx];
            assign match[w] = valid[w]
                            && (way_hash[w][set_idx] == hash_acc)
                            && (way_nlen[w][set_idx] == name_len)
                            && (way_name[w][set_idx] == name_buf);
            assign empty[w] = ~valid[w];
        end
    endgenerate

    // Priority encoder for first empty way
    wire [1:0] empty_way;
    wire       has_empty;
    assign empty_way = empty[0] ? 2'd0 :
                       empty[1] ? 2'd1 :
                       empty[2] ? 2'd2 :
                       empty[3] ? 2'd3 : 2'd0;
    assign has_empty = |empty;

    // Priority encoder for first matching way
    wire [1:0] match_way;
    wire       has_match;
    assign match_way = match[0] ? 2'd0 :
                       match[1] ? 2'd1 :
                       match[2] ? 2'd2 :
                       match[3] ? 2'd3 : 2'd0;
    assign has_match = |match;

    // Clear counter
    reg [SET_BITS-1:0] clr_idx;

    // ====================================================================
    // Main FSM
    // ====================================================================
    always @(posedge clk) begin
        if (rst) begin
            state     <= D_IDLE;
            done      <= 1'b0;
            bus_req   <= 1'b0;
            bus_wr    <= 1'b0;
            flag_z    <= 1'b0;
            flag_v    <= 1'b0;
            xt_out    <= 64'd0;
            bcast_valid <= 1'b0;
            snoop_ack <= 1'b0;
        end else begin
            done      <= 1'b0;
            bus_req   <= 1'b0;
            snoop_ack <= 1'b0;

            // -----------------------------------------------------------
            // Snoop: handle broadcast from other cores (any state)
            // -----------------------------------------------------------
            if (snoop_valid && state != D_INSERT && state != D_CLEAR) begin
                // Insert snooped entry into local table
                // Compute set from snoop_hash[5:0]
                if (|({~way_valid[0][snoop_hash[SET_BITS-1:0]],
                       ~way_valid[1][snoop_hash[SET_BITS-1:0]],
                       ~way_valid[2][snoop_hash[SET_BITS-1:0]],
                       ~way_valid[3][snoop_hash[SET_BITS-1:0]]})) begin
                    // Find first empty way in target set
                    if (!way_valid[0][snoop_hash[SET_BITS-1:0]]) begin
                        way_valid[0][snoop_hash[SET_BITS-1:0]] <= 1'b1;
                        way_hash[0][snoop_hash[SET_BITS-1:0]]  <= snoop_hash;
                        way_nlen[0][snoop_hash[SET_BITS-1:0]]  <= snoop_name_len;
                        way_name[0][snoop_hash[SET_BITS-1:0]]  <= snoop_name;
                        way_xt[0][snoop_hash[SET_BITS-1:0]]    <= snoop_xt;
                    end else if (!way_valid[1][snoop_hash[SET_BITS-1:0]]) begin
                        way_valid[1][snoop_hash[SET_BITS-1:0]] <= 1'b1;
                        way_hash[1][snoop_hash[SET_BITS-1:0]]  <= snoop_hash;
                        way_nlen[1][snoop_hash[SET_BITS-1:0]]  <= snoop_name_len;
                        way_name[1][snoop_hash[SET_BITS-1:0]]  <= snoop_name;
                        way_xt[1][snoop_hash[SET_BITS-1:0]]    <= snoop_xt;
                    end else if (!way_valid[2][snoop_hash[SET_BITS-1:0]]) begin
                        way_valid[2][snoop_hash[SET_BITS-1:0]] <= 1'b1;
                        way_hash[2][snoop_hash[SET_BITS-1:0]]  <= snoop_hash;
                        way_nlen[2][snoop_hash[SET_BITS-1:0]]  <= snoop_name_len;
                        way_name[2][snoop_hash[SET_BITS-1:0]]  <= snoop_name;
                        way_xt[2][snoop_hash[SET_BITS-1:0]]    <= snoop_xt;
                    end else begin
                        way_valid[3][snoop_hash[SET_BITS-1:0]] <= 1'b1;
                        way_hash[3][snoop_hash[SET_BITS-1:0]]  <= snoop_hash;
                        way_nlen[3][snoop_hash[SET_BITS-1:0]]  <= snoop_name_len;
                        way_name[3][snoop_hash[SET_BITS-1:0]]  <= snoop_name;
                        way_xt[3][snoop_hash[SET_BITS-1:0]]    <= snoop_xt;
                    end
                end
                snoop_ack <= 1'b1;
            end

            case (state)

            // ============================================================
            // IDLE — wait for start pulse
            // ============================================================
            D_IDLE: begin
                if (start) begin
                    cur_op   <= op;
                    flag_z   <= 1'b0;
                    flag_v   <= 1'b0;
                    xt_out   <= 64'd0;

                    if (op == 4'h03) begin
                        // DCLR — clear all valid bits
                        clr_idx <= {SET_BITS{1'b0}};
                        state   <= D_CLEAR;
                    end else begin
                        // DFIND / DINS / DDEL — read counted-string
                        name_ptr <= name_addr;  // points to length byte
                        save_xt  <= xt_in;
                        state    <= D_RD_LEN;
                    end
                end
            end

            // ============================================================
            // RD_LEN — issue bus read for length byte of counted-string
            // ============================================================
            D_RD_LEN: begin
                bus_req  <= 1'b1;
                bus_wr   <= 1'b0;
                bus_addr <= name_ptr;
                state    <= D_WAIT_LEN;
            end

            // ============================================================
            // WAIT_LEN — receive length byte
            // ============================================================
            D_WAIT_LEN: begin
                bus_req  <= 1'b1;
                bus_wr   <= 1'b0;
                bus_addr <= name_ptr;
                if (bus_ack) begin
                    bus_req  <= 1'b0;
                    name_len <= (bus_rdata[4:0] > MAX_NAME[4:0])
                              ? MAX_NAME[4:0] : bus_rdata[4:0];
                    byte_idx <= 5'd0;
                    name_buf <= {248{1'b0}};
                    hash_acc <= FNV_OFFSET;
                    name_ptr <= name_ptr + 64'd1;  // advance past length byte
                    if (bus_rdata[4:0] == 5'd0) begin
                        // Zero-length name — can't match anything
                        set_idx <= FNV_OFFSET[SET_BITS-1:0];
                        state   <= D_LOOKUP;
                    end else begin
                        state <= D_RD_NAME;
                    end
                end
            end

            // ============================================================
            // RD_NAME — issue bus read for next name byte
            // ============================================================
            D_RD_NAME: begin
                bus_req  <= 1'b1;
                bus_wr   <= 1'b0;
                bus_addr <= name_ptr;
                state    <= D_WAIT_NM;
            end

            // ============================================================
            // WAIT_NM — receive name byte, update hash, buffer name
            // ============================================================
            D_WAIT_NM: begin
                bus_req  <= 1'b1;
                bus_wr   <= 1'b0;
                bus_addr <= name_ptr;
                if (bus_ack) begin
                    bus_req <= 1'b0;

                    // FNV-1a: hash ^= byte; hash *= prime
                    hash_acc <= (hash_acc ^ {24'd0, bus_rdata[7:0]})
                              * FNV_PRIME;

                    // Store byte into name buffer (MSB-first packing)
                    name_buf <= name_buf | ({{240{1'b0}}, bus_rdata[7:0]}
                                << ((MAX_NAME[4:0] - 5'd1 - byte_idx) * 8));

                    byte_idx <= byte_idx + 5'd1;
                    name_ptr <= name_ptr + 64'd1;

                    if (byte_idx + 5'd1 == name_len) begin
                        // All bytes read — compute set index from hash
                        set_idx <= ((hash_acc ^ {24'd0, bus_rdata[7:0]})
                                    * FNV_PRIME) >> 0;
                        state   <= D_LOOKUP;
                    end else begin
                        state <= D_RD_NAME;
                    end
                end
            end

            // ============================================================
            // LOOKUP — compare hash+name against all 4 ways
            // ============================================================
            D_LOOKUP: begin
                case (cur_op)
                    4'h00: begin  // DFIND
                        if (has_match) begin
                            flag_z <= 1'b1;
                            // Explicit MUX — avoid way_xt[match_way][set_idx]
                            if (match[0])
                                xt_out <= way_xt[0][set_idx];
                            else if (match[1])
                                xt_out <= way_xt[1][set_idx];
                            else if (match[2])
                                xt_out <= way_xt[2][set_idx];
                            else
                                xt_out <= way_xt[3][set_idx];
                        end else begin
                            flag_z <= 1'b0;
                        end
                        state <= D_DONE;
                    end
                    4'h01: begin  // DINS
                        state <= D_INSERT;
                    end
                    4'h02: begin  // DDEL
                        state <= D_DELETE;
                    end
                    default: state <= D_DONE;
                endcase
            end

            // ============================================================
            // INSERT — write entry to an empty way (or overwrite match)
            // ============================================================
            D_INSERT: begin
                if (has_match) begin
                    // Update existing entry's XT — explicit way selection
                    if (match[0])
                        way_xt[0][set_idx] <= save_xt;
                    else if (match[1])
                        way_xt[1][set_idx] <= save_xt;
                    else if (match[2])
                        way_xt[2][set_idx] <= save_xt;
                    else
                        way_xt[3][set_idx] <= save_xt;
                    flag_z <= 1'b1;
                    flag_v <= 1'b0;
                    state  <= D_BCAST;
                end else if (has_empty) begin
                    // Insert into first empty way — explicit way selection
                    if (empty[0]) begin
                        way_valid[0][set_idx] <= 1'b1;
                        way_hash[0][set_idx]  <= hash_acc;
                        way_nlen[0][set_idx]  <= name_len;
                        way_name[0][set_idx]  <= name_buf;
                        way_xt[0][set_idx]    <= save_xt;
                    end else if (empty[1]) begin
                        way_valid[1][set_idx] <= 1'b1;
                        way_hash[1][set_idx]  <= hash_acc;
                        way_nlen[1][set_idx]  <= name_len;
                        way_name[1][set_idx]  <= name_buf;
                        way_xt[1][set_idx]    <= save_xt;
                    end else if (empty[2]) begin
                        way_valid[2][set_idx] <= 1'b1;
                        way_hash[2][set_idx]  <= hash_acc;
                        way_nlen[2][set_idx]  <= name_len;
                        way_name[2][set_idx]  <= name_buf;
                        way_xt[2][set_idx]    <= save_xt;
                    end else begin
                        way_valid[3][set_idx] <= 1'b1;
                        way_hash[3][set_idx]  <= hash_acc;
                        way_nlen[3][set_idx]  <= name_len;
                        way_name[3][set_idx]  <= name_buf;
                        way_xt[3][set_idx]    <= save_xt;
                    end
                    flag_z <= 1'b1;
                    flag_v <= 1'b0;
                    state  <= D_BCAST;
                end else begin
                    // All 4 ways full — overflow
                    flag_z <= 1'b0;
                    flag_v <= 1'b1;
                    state  <= D_DONE;
                end
            end

            // ============================================================
            // BCAST — broadcast inserted entry to other cores
            // ============================================================
            D_BCAST: begin
                bcast_valid    <= 1'b1;
                bcast_hash     <= hash_acc;
                bcast_name_len <= name_len;
                bcast_name     <= name_buf;
                bcast_xt       <= save_xt;
                if (bcast_ack) begin
                    bcast_valid <= 1'b0;
                    state       <= D_DONE;
                end
            end

            // ============================================================
            // DELETE — invalidate matching entry
            // ============================================================
            D_DELETE: begin
                if (has_match) begin
                    // Explicit way selection
                    if (match[0])
                        way_valid[0][set_idx] <= 1'b0;
                    else if (match[1])
                        way_valid[1][set_idx] <= 1'b0;
                    else if (match[2])
                        way_valid[2][set_idx] <= 1'b0;
                    else
                        way_valid[3][set_idx] <= 1'b0;
                    flag_z <= 1'b1;
                end else begin
                    flag_z <= 1'b0;
                end
                state <= D_DONE;
            end

            // ============================================================
            // CLEAR — bulk-zero all valid bits (iterate all sets)
            // ============================================================
            D_CLEAR: begin
                way_valid[0][clr_idx] <= 1'b0;
                way_valid[1][clr_idx] <= 1'b0;
                way_valid[2][clr_idx] <= 1'b0;
                way_valid[3][clr_idx] <= 1'b0;
                if (clr_idx == {SET_BITS{1'b1}}) begin
                    state <= D_DONE;
                end else begin
                    clr_idx <= clr_idx + 1;
                end
            end

            // ============================================================
            // DONE — pulse done, return to idle
            // ============================================================
            D_DONE: begin
                done  <= 1'b1;
                state <= D_IDLE;
            end

            endcase
        end
    end

    // ====================================================================
    // BRAM initialisation (simulation only)
    // ====================================================================
    integer si, wi;
    initial begin
        for (si = 0; si < NSETS; si = si + 1) begin
            for (wi = 0; wi < NWAYS; wi = wi + 1) begin
                way_valid[wi][si] = 1'b0;
                way_hash[wi][si]  = 32'd0;
                way_nlen[wi][si]  = 5'd0;
                way_name[wi][si]  = {248{1'b0}};
                way_xt[wi][si]    = 64'd0;
            end
        end
    end

endmodule
