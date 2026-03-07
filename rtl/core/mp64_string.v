// ============================================================================
// mp64_string.v — Forth-Aware String Engine (EXT.STRING, prefix F9)
// ============================================================================
//
// Tightly-coupled CPU sub-module implementing block memory operations:
//   CMOVE  (00) — forward byte copy,   len in R0
//   CMOVE> (01) — backward byte copy,  len in R0  (overlap-safe)
//   BFILL  (02) — byte fill from D,    len in Rn
//   BCOMP  (03) — byte compare,        len in R0
//   BSRCH  (04) — byte search for D,   len in R0
//
// Uses the CPU's existing bus master during stall (CPU_STRING state).
// Byte-at-a-time V1; 64-bit aligned burst is a future optimisation.
//
// Verilog-2001.  No vendor primitives.
//

module mp64_string (
    input  wire        clk,
    input  wire        rst,

    // --- CPU handshake ---
    input  wire        start,         // pulse: begin operation
    input  wire [3:0]  op,            // sub-op (00–04)
    input  wire [63:0] src_addr,      // Rs value
    input  wire [63:0] dst_addr,      // Rd value
    input  wire [63:0] length,        // R0 or Rn (byte count)
    input  wire [7:0]  fill_byte,     // D[7:0] for BFILL / BSRCH needle
    output reg         done,          // pulse: operation complete
    output reg  [63:0] out_src,       // updated Rs
    output reg  [63:0] out_dst,       // updated Rd
    output reg  [63:0] out_len,       // updated R0/Rn (0 after copy/fill)
    output reg  [63:0] out_result,    // BSRCH: match offset in Rs
    output reg         flag_z,        // BCOMP: equal; BSRCH: found
    output reg         flag_g,        // BCOMP: Rd[i] > Rs[i] at mismatch

    // --- Bus master (active only during stall) ---
    output reg         bus_req,
    output reg         bus_wr,
    output reg  [63:0] bus_addr,
    output reg  [63:0] bus_wdata,
    input  wire        bus_ack,
    input  wire [63:0] bus_rdata
);

    // ====================================================================
    // FSM states
    // ====================================================================
    localparam [2:0] S_IDLE    = 3'd0;
    localparam [2:0] S_READ    = 3'd1;   // issue bus read  (src or haystack)
    localparam [2:0] S_WAIT_R  = 3'd2;   // wait ack for 1st read
    localparam [2:0] S_READ2   = 3'd3;   // 2nd read (BCOMP: dst byte)
    localparam [2:0] S_WAIT_R2 = 3'd4;   // wait ack for 2nd read
    localparam [2:0] S_WRITE   = 3'd5;   // issue bus write
    localparam [2:0] S_WAIT_W  = 3'd6;   // wait ack for write
    localparam [2:0] S_DONE    = 3'd7;

    // ====================================================================
    // Working registers
    // ====================================================================
    reg [2:0]  state;
    reg [3:0]  cur_op;
    reg [63:0] s_ptr;           // working source pointer
    reg [63:0] d_ptr;           // working dest pointer
    reg [63:0] cnt;             // remaining bytes
    reg [63:0] orig_len;        // original length (for BSRCH offset)
    reg [7:0]  needle;          // latched fill/search byte
    reg [7:0]  rd_byte;         // byte just read (src side)

    // ====================================================================
    // Main FSM
    // ====================================================================
    always @(posedge clk) begin
        if (rst) begin
            state    <= S_IDLE;
            done     <= 1'b0;
            bus_req  <= 1'b0;
            bus_wr   <= 1'b0;
            bus_addr <= 64'd0;
            bus_wdata<= 64'd0;
            flag_z   <= 1'b0;
            flag_g   <= 1'b0;
        end else begin
            done    <= 1'b0;
            bus_req <= 1'b0;

            case (state)

            // ============================================================
            // IDLE — wait for start pulse
            // ============================================================
            S_IDLE: begin
                if (start) begin
                    cur_op   <= op;
                    cnt      <= length;
                    orig_len <= length;
                    needle   <= fill_byte;
                    flag_z   <= 1'b0;
                    flag_g   <= 1'b0;

                    // Pointer setup: CMOVE> starts from end
                    if (op == 4'h01) begin
                        s_ptr <= src_addr + length - 64'd1;
                        d_ptr <= dst_addr + length - 64'd1;
                    end else begin
                        s_ptr <= src_addr;
                        d_ptr <= dst_addr;
                    end

                    // Zero-length: immediate completion
                    if (length == 64'd0) begin
                        out_src    <= src_addr;
                        out_dst    <= dst_addr;
                        out_len    <= 64'd0;
                        out_result <= 64'd0;
                        // BCOMP zero-length → Z=1 (equal by convention)
                        flag_z     <= (op == 4'h03);
                        state      <= S_DONE;
                    end else begin
                        case (op)
                            4'h00,
                            4'h01: state <= S_READ;     // CMOVE, CMOVE>: read src
                            4'h02: state <= S_WRITE;    // BFILL: write directly
                            4'h03: state <= S_READ;     // BCOMP: read src first
                            4'h04: state <= S_READ;     // BSRCH: read haystack
                            default: begin              // reserved → immediate done
                                out_src <= src_addr;
                                out_dst <= dst_addr;
                                out_len <= length;
                                out_result <= 64'd0;
                                state   <= S_DONE;
                            end
                        endcase
                    end
                end
            end

            // ============================================================
            // READ — issue bus read
            // ============================================================
            S_READ: begin
                bus_req  <= 1'b1;
                bus_wr   <= 1'b0;
                // BSRCH reads from d_ptr (haystack at Rd)
                // Others read from s_ptr (source at Rs)
                bus_addr <= (cur_op == 4'h04) ? d_ptr : s_ptr;
                state    <= S_WAIT_R;
            end

            // ============================================================
            // WAIT_R — wait for read ack
            // ============================================================
            S_WAIT_R: begin
                bus_req <= 1'b1;
                bus_wr  <= 1'b0;
                bus_addr <= (cur_op == 4'h04) ? d_ptr : s_ptr;
                if (bus_ack) begin
                    bus_req <= 1'b0;
                    rd_byte <= bus_rdata[7:0];

                    case (cur_op)
                        4'h00, 4'h01: begin
                            // CMOVE / CMOVE> — got src byte, now write to dst
                            state <= S_WRITE;
                        end
                        4'h03: begin
                            // BCOMP — got src byte, now read dst byte
                            state <= S_READ2;
                        end
                        4'h04: begin
                            // BSRCH — check if haystack byte matches needle
                            if (bus_rdata[7:0] == needle) begin
                                // Found!
                                flag_z     <= 1'b1;
                                out_result <= orig_len - cnt;   // offset
                                out_src    <= orig_len - cnt;   // Rs ← offset
                                out_dst    <= d_ptr;
                                out_len    <= cnt;
                                state      <= S_DONE;
                            end else begin
                                d_ptr <= d_ptr + 64'd1;
                                cnt   <= cnt - 64'd1;
                                if (cnt == 64'd1) begin
                                    // Exhausted — not found
                                    flag_z     <= 1'b0;
                                    out_result <= orig_len;     // Rs ← original length
                                    out_src    <= orig_len;
                                    out_dst    <= d_ptr + 64'd1;
                                    out_len    <= 64'd0;
                                    state      <= S_DONE;
                                end else begin
                                    state <= S_READ;
                                end
                            end
                        end
                        default: state <= S_DONE;
                    endcase
                end
            end

            // ============================================================
            // READ2 — BCOMP: issue bus read for dst byte
            // ============================================================
            S_READ2: begin
                bus_req  <= 1'b1;
                bus_wr   <= 1'b0;
                bus_addr <= d_ptr;
                state    <= S_WAIT_R2;
            end

            // ============================================================
            // WAIT_R2 — BCOMP: compare src byte (rd_byte) vs dst byte
            // ============================================================
            S_WAIT_R2: begin
                bus_req  <= 1'b1;
                bus_wr   <= 1'b0;
                bus_addr <= d_ptr;
                if (bus_ack) begin
                    bus_req <= 1'b0;
                    if (rd_byte != bus_rdata[7:0]) begin
                        // Mismatch — done
                        flag_z  <= 1'b0;
                        flag_g  <= (bus_rdata[7:0] > rd_byte);  // dst > src
                        out_src <= s_ptr;
                        out_dst <= d_ptr;
                        out_len <= cnt;
                        state   <= S_DONE;
                    end else begin
                        // Match — advance
                        s_ptr <= s_ptr + 64'd1;
                        d_ptr <= d_ptr + 64'd1;
                        cnt   <= cnt - 64'd1;
                        if (cnt == 64'd1) begin
                            // All bytes matched
                            flag_z  <= 1'b1;
                            flag_g  <= 1'b0;
                            out_src <= s_ptr + 64'd1;
                            out_dst <= d_ptr + 64'd1;
                            out_len <= 64'd0;
                            state   <= S_DONE;
                        end else begin
                            state <= S_READ;
                        end
                    end
                end
            end

            // ============================================================
            // WRITE — issue bus write
            // ============================================================
            S_WRITE: begin
                bus_req  <= 1'b1;
                bus_wr   <= 1'b1;
                bus_addr <= d_ptr;
                // BFILL uses needle; CMOVE/CMOVE> uses rd_byte
                bus_wdata <= (cur_op == 4'h02) ? {56'd0, needle} : {56'd0, rd_byte};
                state     <= S_WAIT_W;
            end

            // ============================================================
            // WAIT_W — wait for write ack, advance pointers
            // ============================================================
            S_WAIT_W: begin
                bus_req  <= 1'b1;
                bus_wr   <= 1'b1;
                bus_addr <= d_ptr;
                bus_wdata <= (cur_op == 4'h02) ? {56'd0, needle} : {56'd0, rd_byte};
                if (bus_ack) begin
                    bus_req <= 1'b0;
                    cnt     <= cnt - 64'd1;

                    if (cur_op == 4'h01) begin
                        // CMOVE> — decrement
                        s_ptr <= s_ptr - 64'd1;
                        d_ptr <= d_ptr - 64'd1;
                    end else begin
                        // CMOVE / BFILL — increment
                        s_ptr <= s_ptr + 64'd1;
                        d_ptr <= d_ptr + 64'd1;
                    end

                    if (cnt == 64'd1) begin
                        // Last byte written — compute final register values
                        if (cur_op == 4'h01) begin
                            // CMOVE>: final ptrs = original start - 1
                            // But Forth convention: post-pointers = base + length
                            out_src <= src_addr + orig_len;
                            out_dst <= dst_addr + orig_len;
                        end else begin
                            // CMOVE/BFILL: ptrs already at base + length
                            out_src <= s_ptr + 64'd1;
                            out_dst <= d_ptr + 64'd1;
                        end
                        out_len <= 64'd0;
                        state   <= S_DONE;
                    end else begin
                        // More bytes — loop
                        if (cur_op == 4'h02)
                            state <= S_WRITE;       // BFILL: no read needed
                        else
                            state <= S_READ;        // CMOVE/CMOVE>: read next byte
                    end
                end
            end

            // ============================================================
            // DONE — pulse done, return to idle
            // ============================================================
            S_DONE: begin
                done  <= 1'b1;
                state <= S_IDLE;
            end

            endcase
        end
    end

endmodule
