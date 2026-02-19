// ============================================================================
// mp64_sha256.v — SHA-256 Hardware Accelerator
// ============================================================================
//
// MMIO base: 0x940 (64 bytes).
//
// SHA-256 (FIPS 180-4):  256-bit hash, 512-bit (64-byte) blocks.
//   64 rounds per block, operating on 32-bit words.
//   Message schedule expands 16 → 64 words.
//
// Interface mirrors mp64_sha3.v: CMD/STATUS/DIN/DOUT pattern.
//   Write bytes to DIN → FINAL pads + compresses → DOUT holds 32-byte hash.
//
// Register map (offsets from SHA256_BASE = 0x940):
//   0x00  CMD      (W)  0=NOP, 1=INIT, 3=FINAL
//   0x08  STATUS   (R)  bit 0=busy, bit 1=done
//   0x10  DIN      (W)  byte input (auto-compresses at 64-byte boundary)
//   0x18..0x37  DOUT (R) 32-byte hash output (8 × 32-bit, big-endian words)
//

`include "mp64_pkg.vh"

module mp64_sha256 (
    input  wire        clk,
    input  wire        rst_n,

    // MMIO interface
    input  wire        req,
    input  wire [5:0]  addr,       // offset within SHA256 block
    input  wire [63:0] wdata,
    input  wire        wen,
    output reg  [63:0] rdata,
    output reg         ack,

    // Interrupt
    output reg         irq
);

    // ========================================================================
    // SHA-256 Constants (K) — 64 × 32-bit
    // ========================================================================
    function [31:0] K;
        input [5:0] i;
        begin
            case (i)
                6'd0:  K = 32'h428a2f98;  6'd1:  K = 32'h71374491;
                6'd2:  K = 32'hb5c0fbcf;  6'd3:  K = 32'he9b5dba5;
                6'd4:  K = 32'h3956c25b;  6'd5:  K = 32'h59f111f1;
                6'd6:  K = 32'h923f82a4;  6'd7:  K = 32'hab1c5ed5;
                6'd8:  K = 32'hd807aa98;  6'd9:  K = 32'h12835b01;
                6'd10: K = 32'h243185be;  6'd11: K = 32'h550c7dc3;
                6'd12: K = 32'h72be5d74;  6'd13: K = 32'h80deb1fe;
                6'd14: K = 32'h9bdc06a7;  6'd15: K = 32'hc19bf174;
                6'd16: K = 32'he49b69c1;  6'd17: K = 32'hefbe4786;
                6'd18: K = 32'h0fc19dc6;  6'd19: K = 32'h240ca1cc;
                6'd20: K = 32'h2de92c6f;  6'd21: K = 32'h4a7484aa;
                6'd22: K = 32'h5cb0a9dc;  6'd23: K = 32'h76f988da;
                6'd24: K = 32'h983e5152;  6'd25: K = 32'ha831c66d;
                6'd26: K = 32'hb00327c8;  6'd27: K = 32'hbf597fc7;
                6'd28: K = 32'hc6e00bf3;  6'd29: K = 32'hd5a79147;
                6'd30: K = 32'h06ca6351;  6'd31: K = 32'h14292967;
                6'd32: K = 32'h27b70a85;  6'd33: K = 32'h2e1b2138;
                6'd34: K = 32'h4d2c6dfc;  6'd35: K = 32'h53380d13;
                6'd36: K = 32'h650a7354;  6'd37: K = 32'h766a0abb;
                6'd38: K = 32'h81c2c92e;  6'd39: K = 32'h92722c85;
                6'd40: K = 32'ha2bfe8a1;  6'd41: K = 32'ha81a664b;
                6'd42: K = 32'hc24b8b70;  6'd43: K = 32'hc76c51a3;
                6'd44: K = 32'hd192e819;  6'd45: K = 32'hd6990624;
                6'd46: K = 32'hf40e3585;  6'd47: K = 32'h106aa070;
                6'd48: K = 32'h19a4c116;  6'd49: K = 32'h1e376c08;
                6'd50: K = 32'h2748774c;  6'd51: K = 32'h34b0bcb5;
                6'd52: K = 32'h391c0cb3;  6'd53: K = 32'h4ed8aa4a;
                6'd54: K = 32'h5b9cca4f;  6'd55: K = 32'h682e6ff3;
                6'd56: K = 32'h748f82ee;  6'd57: K = 32'h78a5636f;
                6'd58: K = 32'h84c87814;  6'd59: K = 32'h8cc70208;
                6'd60: K = 32'h90befffa;  6'd61: K = 32'ha4506ceb;
                6'd62: K = 32'hbef9a3f7;  6'd63: K = 32'hc67178f2;
                default: K = 32'd0;
            endcase
        end
    endfunction

    // ========================================================================
    // SHA-256 Σ/σ functions (32-bit rotate right + shift right)
    // ========================================================================
    function [31:0] rotr32;
        input [31:0] val;
        input [4:0]  amt;
        begin
            rotr32 = (val >> amt) | (val << (5'd0 - amt));
        end
    endfunction

    // Σ0(a) = ROTR(2,a) ^ ROTR(13,a) ^ ROTR(22,a)
    function [31:0] big_sigma0;
        input [31:0] x;
        begin
            big_sigma0 = rotr32(x, 5'd2) ^ rotr32(x, 5'd13) ^ rotr32(x, 5'd22);
        end
    endfunction

    // Σ1(e) = ROTR(6,e) ^ ROTR(11,e) ^ ROTR(25,e)
    function [31:0] big_sigma1;
        input [31:0] x;
        begin
            big_sigma1 = rotr32(x, 5'd6) ^ rotr32(x, 5'd11) ^ rotr32(x, 5'd25);
        end
    endfunction

    // σ0(x) = ROTR(7,x) ^ ROTR(18,x) ^ SHR(3,x)
    function [31:0] small_sigma0;
        input [31:0] x;
        begin
            small_sigma0 = rotr32(x, 5'd7) ^ rotr32(x, 5'd18) ^ (x >> 3);
        end
    endfunction

    // σ1(x) = ROTR(17,x) ^ ROTR(19,x) ^ SHR(10,x)
    function [31:0] small_sigma1;
        input [31:0] x;
        begin
            small_sigma1 = rotr32(x, 5'd17) ^ rotr32(x, 5'd19) ^ (x >> 10);
        end
    endfunction

    // Ch(e,f,g) = (e AND f) XOR (NOT e AND g)
    function [31:0] ch;
        input [31:0] e, f, g;
        begin
            ch = (e & f) ^ (~e & g);
        end
    endfunction

    // Maj(a,b,c) = (a AND b) XOR (a AND c) XOR (b AND c)
    function [31:0] maj;
        input [31:0] a, b, c;
        begin
            maj = (a & b) ^ (a & c) ^ (b & c);
        end
    endfunction

    // ========================================================================
    // State & working variables
    // ========================================================================
    // Hash state H[0..7]
    reg [31:0] H [0:7];

    // Working variables a..h
    reg [31:0] wa, wb, wc, wd, we, wf, wg, wh;

    // Message schedule W[0..15] — 16-entry window, expanded on-the-fly
    reg [31:0] W [0:15];

    // Input buffer (64 bytes = one block)
    reg [7:0]  din_buf [0:63];
    reg [6:0]  din_ptr;          // byte counter (0..63)

    // Total message length in bits (for padding)
    reg [63:0] msg_len_bits;

    // Round counter
    reg [6:0]  round_cnt;

    // FSM
    localparam [2:0] S_IDLE    = 3'd0;
    localparam [2:0] S_LOAD    = 3'd1;   // Load W[0..15] from block
    localparam [2:0] S_ROUND   = 3'd2;   // Execute 64 rounds
    localparam [2:0] S_DONE    = 3'd3;   // Add working vars to H, signal done
    localparam [2:0] S_PAD     = 3'd4;   // Padding for FINAL

    reg [2:0]  sha_state;
    reg        busy;
    reg        done_flag;
    reg        do_final;
    reg [7:0]  pad_phase;       // Sub-phase during padding

    // Output digest (32 bytes, big-endian)
    reg [7:0]  digest [0:31];

    integer ki;

    // ========================================================================
    // One round combinational — compute T1, T2, shift registers
    // ========================================================================
    // Current W value for the round
    wire [31:0] w_cur = W[round_cnt[3:0]];

    wire [31:0] T1 = wh + big_sigma1(we) + ch(we, wf, wg) + K(round_cnt[5:0]) + w_cur;
    wire [31:0] T2 = big_sigma0(wa) + maj(wa, wb, wc);

    // Message schedule expansion: W[i] = σ1(W[i-2]) + W[i-7] + σ0(W[i-15]) + W[i-16]
    // When round_cnt >= 16, before using W[round_cnt[3:0]], we expand it.
    wire [31:0] w_new = small_sigma1(W[(round_cnt[3:0] + 4'd14) & 4'hF]) +
                        W[(round_cnt[3:0] + 4'd9)  & 4'hF] +
                        small_sigma0(W[(round_cnt[3:0] + 4'd1)  & 4'hF]) +
                        W[round_cnt[3:0]];

    // ========================================================================
    // Main FSM
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            sha_state  <= S_IDLE;
            busy       <= 1'b0;
            done_flag  <= 1'b0;
            irq        <= 1'b0;
            din_ptr    <= 7'd0;
            msg_len_bits <= 64'd0;
            round_cnt  <= 7'd0;
            do_final   <= 1'b0;
            pad_phase  <= 8'd0;
            for (ki = 0; ki < 8; ki = ki + 1)
                H[ki] <= 32'd0;
            for (ki = 0; ki < 16; ki = ki + 1)
                W[ki] <= 32'd0;
            for (ki = 0; ki < 64; ki = ki + 1)
                din_buf[ki] <= 8'd0;
            for (ki = 0; ki < 32; ki = ki + 1)
                digest[ki] <= 8'd0;
            wa <= 32'd0; wb <= 32'd0; wc <= 32'd0; wd <= 32'd0;
            we <= 32'd0; wf <= 32'd0; wg <= 32'd0; wh <= 32'd0;
        end else begin
            irq <= 1'b0;

            case (sha_state)
                S_IDLE: begin
                    // Wait for commands via MMIO
                end

                S_LOAD: begin
                    // Load W[0..15] from din_buf (big-endian 32-bit words)
                    for (ki = 0; ki < 16; ki = ki + 1) begin
                        W[ki] <= {din_buf[ki*4], din_buf[ki*4+1],
                                  din_buf[ki*4+2], din_buf[ki*4+3]};
                    end
                    // Initialize working variables from current hash
                    wa <= H[0]; wb <= H[1]; wc <= H[2]; wd <= H[3];
                    we <= H[4]; wf <= H[5]; wg <= H[6]; wh <= H[7];
                    round_cnt <= 7'd0;
                    sha_state <= S_ROUND;
                end

                S_ROUND: begin
                    // Message schedule expansion for rounds 16+
                    if (round_cnt >= 7'd16) begin
                        W[round_cnt[3:0]] <= w_new;
                    end

                    // Update working variables
                    wh <= wg;
                    wg <= wf;
                    wf <= we;
                    we <= wd + T1;
                    wd <= wc;
                    wc <= wb;
                    wb <= wa;
                    wa <= T1 + T2;

                    if (round_cnt == 7'd63) begin
                        sha_state <= S_DONE;
                    end else begin
                        round_cnt <= round_cnt + 7'd1;
                    end
                end

                S_DONE: begin
                    // Add compressed chunk to current hash value
                    H[0] <= H[0] + wa;
                    H[1] <= H[1] + wb;
                    H[2] <= H[2] + wc;
                    H[3] <= H[3] + wd;
                    H[4] <= H[4] + we;
                    H[5] <= H[5] + wf;
                    H[6] <= H[6] + wg;
                    H[7] <= H[7] + wh;

                    // Clear input buffer
                    for (ki = 0; ki < 64; ki = ki + 1)
                        din_buf[ki] <= 8'd0;
                    din_ptr <= 7'd0;

                    if (do_final) begin
                        // Build digest output (big-endian 32-bit words)
                        // Need to use updated H values (available next cycle)
                        do_final <= 1'b0;
                        busy     <= 1'b0;
                        done_flag <= 1'b1;
                        irq      <= 1'b1;

                        // Extract digest bytes (big-endian per FIPS 180-4)
                        digest[0]  <= (H[0] + wa) >> 24; digest[1]  <= (H[0] + wa) >> 16;
                        digest[2]  <= (H[0] + wa) >> 8;  digest[3]  <= (H[0] + wa);
                        digest[4]  <= (H[1] + wb) >> 24; digest[5]  <= (H[1] + wb) >> 16;
                        digest[6]  <= (H[1] + wb) >> 8;  digest[7]  <= (H[1] + wb);
                        digest[8]  <= (H[2] + wc) >> 24; digest[9]  <= (H[2] + wc) >> 16;
                        digest[10] <= (H[2] + wc) >> 8;  digest[11] <= (H[2] + wc);
                        digest[12] <= (H[3] + wd) >> 24; digest[13] <= (H[3] + wd) >> 16;
                        digest[14] <= (H[3] + wd) >> 8;  digest[15] <= (H[3] + wd);
                        digest[16] <= (H[4] + we) >> 24; digest[17] <= (H[4] + we) >> 16;
                        digest[18] <= (H[4] + we) >> 8;  digest[19] <= (H[4] + we);
                        digest[20] <= (H[5] + wf) >> 24; digest[21] <= (H[5] + wf) >> 16;
                        digest[22] <= (H[5] + wf) >> 8;  digest[23] <= (H[5] + wf);
                        digest[24] <= (H[6] + wg) >> 24; digest[25] <= (H[6] + wg) >> 16;
                        digest[26] <= (H[6] + wg) >> 8;  digest[27] <= (H[6] + wg);
                        digest[28] <= (H[7] + wh) >> 24; digest[29] <= (H[7] + wh) >> 16;
                        digest[30] <= (H[7] + wh) >> 8;  digest[31] <= (H[7] + wh);
                    end else begin
                        busy <= 1'b0;
                        sha_state <= S_IDLE;
                    end
                end

                S_PAD: begin
                    // SHA-256 padding: append 0x80, zeros, 64-bit big-endian length
                    case (pad_phase)
                        8'd0: begin
                            // Append 0x80 byte
                            din_buf[din_ptr] <= 8'h80;
                            // Check if we have room for length (need 8 bytes at end)
                            // If din_ptr >= 56, we need two blocks
                            if (din_ptr >= 7'd56) begin
                                // Fill rest with zeros, compress this block, then
                                // do a second block with just the length
                                pad_phase <= 8'd10;  // two-block path
                            end else begin
                                pad_phase <= 8'd1;   // single-block path
                            end
                        end

                        // Single-block path: pad zeros then length
                        8'd1: begin
                            // Zero-fill bytes [din_ptr+1 .. 55]
                            for (ki = 0; ki < 64; ki = ki + 1) begin
                                if (ki > din_ptr && ki < 56)
                                    din_buf[ki] <= 8'd0;
                            end
                            // Write 64-bit big-endian length at bytes [56..63]
                            din_buf[56] <= msg_len_bits[63:56];
                            din_buf[57] <= msg_len_bits[55:48];
                            din_buf[58] <= msg_len_bits[47:40];
                            din_buf[59] <= msg_len_bits[39:32];
                            din_buf[60] <= msg_len_bits[31:24];
                            din_buf[61] <= msg_len_bits[23:16];
                            din_buf[62] <= msg_len_bits[15:8];
                            din_buf[63] <= msg_len_bits[7:0];
                            do_final <= 1'b1;
                            sha_state <= S_LOAD;
                        end

                        // Two-block path: first compression with padded data
                        8'd10: begin
                            // Zero-fill remaining bytes in this block
                            for (ki = 0; ki < 64; ki = ki + 1) begin
                                if (ki > din_ptr)
                                    din_buf[ki] <= 8'd0;
                            end
                            do_final <= 1'b0;
                            sha_state <= S_LOAD;
                            pad_phase <= 8'd11;
                        end

                        // Two-block path: after first compression, build length block
                        8'd11: begin
                            // This is reached via S_DONE -> S_IDLE, but we check
                            // pad_phase below in the IDLE handler
                        end

                        default: ;
                    endcase
                end

                default: sha_state <= S_IDLE;
            endcase

            // After S_DONE returns to IDLE with pad_phase == 11,
            // build the final length-only block
            if (sha_state == S_IDLE && pad_phase == 8'd11) begin
                // All zeros except last 8 bytes = bit length
                for (ki = 0; ki < 56; ki = ki + 1)
                    din_buf[ki] <= 8'd0;
                din_buf[56] <= msg_len_bits[63:56];
                din_buf[57] <= msg_len_bits[55:48];
                din_buf[58] <= msg_len_bits[47:40];
                din_buf[59] <= msg_len_bits[39:32];
                din_buf[60] <= msg_len_bits[31:24];
                din_buf[61] <= msg_len_bits[23:16];
                din_buf[62] <= msg_len_bits[15:8];
                din_buf[63] <= msg_len_bits[7:0];
                pad_phase <= 8'd0;
                do_final <= 1'b1;
                sha_state <= S_LOAD;
            end

            // ============================================================
            // Register writes from MMIO
            // ============================================================
            if (req && wen) begin
                case (addr)
                    // CMD register
                    6'h00: begin
                        case (wdata[2:0])
                            3'd1: begin  // INIT — reset to SHA-256 initial hash
                                H[0] <= 32'h6a09e667;
                                H[1] <= 32'hbb67ae85;
                                H[2] <= 32'h3c6ef372;
                                H[3] <= 32'ha54ff53a;
                                H[4] <= 32'h510e527f;
                                H[5] <= 32'h9b05688c;
                                H[6] <= 32'h1f83d9ab;
                                H[7] <= 32'h5be0cd19;
                                din_ptr <= 7'd0;
                                msg_len_bits <= 64'd0;
                                busy <= 1'b0;
                                done_flag <= 1'b0;
                                pad_phase <= 8'd0;
                                do_final <= 1'b0;
                                for (ki = 0; ki < 64; ki = ki + 1)
                                    din_buf[ki] <= 8'd0;
                                for (ki = 0; ki < 32; ki = ki + 1)
                                    digest[ki] <= 8'd0;
                            end
                            3'd3: begin  // FINAL — pad and compress
                                busy <= 1'b1;
                                done_flag <= 1'b0;
                                pad_phase <= 8'd0;
                                sha_state <= S_PAD;
                            end
                            default: ;
                        endcase
                    end

                    // DIN register — write one byte
                    6'h10: begin
                        if (din_ptr < 7'd64) begin
                            din_buf[din_ptr] <= wdata[7:0];
                            din_ptr <= din_ptr + 7'd1;
                            msg_len_bits <= msg_len_bits + 64'd8;
                            // Auto-compress when full block received
                            if (din_ptr == 7'd63) begin
                                busy <= 1'b1;
                                sha_state <= S_LOAD;
                            end
                        end
                    end

                    default: ;
                endcase
            end
        end
    end

    // ========================================================================
    // Register reads
    // ========================================================================
    always @(posedge clk) begin
        ack <= 1'b0;
        if (req) begin
            ack <= 1'b1;
            if (!wen) begin
                case (addr)
                    6'h08: rdata <= {62'd0, done_flag, busy};  // STATUS
                    // DOUT — 32-byte digest at offset 0x18..0x37
                    default: begin
                        if (addr >= 6'h18 && addr < 6'h38) begin
                            rdata <= {56'd0, digest[addr - 6'h18]};
                        end else begin
                            rdata <= 64'd0;
                        end
                    end
                endcase
            end else begin
                rdata <= 64'd0;
            end
        end
    end

endmodule
