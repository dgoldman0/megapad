// ============================================================================
// mp64_sha2_isa.v — Per-Core SHA-256 ISA Compression Engine
// ============================================================================
//
// Multi-cycle SHA-256 compression engine for EXT.CRYPTO (FB 1x) ISA
// instructions.  Instantiated per full core.  64 cycles per compression.
//
// The CPU pre-loads 16 × 32-bit W words and 8 × 32-bit H state before
// asserting `start`.  The engine runs 64 rounds (one per clock) and
// outputs the updated hash state H' = H + working_vars.
//
// SHA-256 state packing into 64-bit ACC registers:
//   ACC0 = {H[0], H[1]}  (a, b)
//   ACC1 = {H[2], H[3]}  (c, d)
//   ACC2 = {H[4], H[5]}  (e, f)
//   ACC3 = {H[6], H[7]}  (g, h)
//
// Note: SHA-384/512 support handled by the emulator only for now.
//       RTL extension to 64-bit words is a future task.
//

`include "mp64_pkg.vh"

module mp64_sha2_isa (
    input  wire        clk,
    input  wire        rst_n,

    // Command
    input  wire        start,          // pulse: begin 64-round compression

    // W[0..15] pre-loaded by CPU (big-endian 32-bit words)
    input  wire [31:0] w_in [0:15],

    // Hash state in (from ACC registers, unpacked to 8 × 32-bit)
    input  wire [31:0] h_in [0:7],

    // Hash state out (updated H' = H + working_vars)
    output reg  [31:0] h_out [0:7],
    output reg         h_we,          // pulse when result valid

    // Status
    output reg         busy,
    output reg         done           // pulse on completion
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
    // SHA-256 helper functions
    // ========================================================================
    function [31:0] rotr32;
        input [31:0] val;
        input [4:0]  amt;
        begin
            rotr32 = (val >> amt) | (val << (5'd0 - amt));
        end
    endfunction

    function [31:0] big_sigma0;
        input [31:0] x;
        begin
            big_sigma0 = rotr32(x, 5'd2) ^ rotr32(x, 5'd13) ^ rotr32(x, 5'd22);
        end
    endfunction

    function [31:0] big_sigma1;
        input [31:0] x;
        begin
            big_sigma1 = rotr32(x, 5'd6) ^ rotr32(x, 5'd11) ^ rotr32(x, 5'd25);
        end
    endfunction

    function [31:0] small_sigma0;
        input [31:0] x;
        begin
            small_sigma0 = rotr32(x, 5'd7) ^ rotr32(x, 5'd18) ^ (x >> 3);
        end
    endfunction

    function [31:0] small_sigma1;
        input [31:0] x;
        begin
            small_sigma1 = rotr32(x, 5'd17) ^ rotr32(x, 5'd19) ^ (x >> 10);
        end
    endfunction

    function [31:0] ch;
        input [31:0] e, f, g;
        begin
            ch = (e & f) ^ (~e & g);
        end
    endfunction

    function [31:0] maj;
        input [31:0] a, b, c;
        begin
            maj = (a & b) ^ (a & c) ^ (b & c);
        end
    endfunction

    // ========================================================================
    // Internal state
    // ========================================================================
    // Working variables
    reg [31:0] wa, wb, wc, wd, we, wf, wg, wh;

    // Message schedule W[0..15] — 16-entry circular window
    reg [31:0] W [0:15];

    // Saved initial hash (for final addition)
    reg [31:0] H_save [0:7];

    // Round counter
    reg [6:0]  round_cnt;

    // FSM
    localparam S_IDLE   = 2'd0;
    localparam S_ROUND  = 2'd1;
    localparam S_DONE   = 2'd2;

    reg [1:0] state;

    integer ki;

    // ========================================================================
    // Combinational round computation
    // ========================================================================
    // Current W value for the round
    wire [31:0] w_cur = W[round_cnt[3:0]];

    // Message schedule expansion: σ1(W[i-2]) + W[i-7] + σ0(W[i-15]) + W[i-16]
    wire [31:0] w_new = small_sigma1(W[(round_cnt[3:0] + 4'd14) & 4'hF]) +
                        W[(round_cnt[3:0] + 4'd9)  & 4'hF] +
                        small_sigma0(W[(round_cnt[3:0] + 4'd1)  & 4'hF]) +
                        W[round_cnt[3:0]];

    // Round function: T1, T2
    wire [31:0] T1 = wh + big_sigma1(we) + ch(we, wf, wg)
                   + K(round_cnt[5:0]) + w_cur;
    wire [31:0] T2 = big_sigma0(wa) + maj(wa, wb, wc);

    // ========================================================================
    // Main FSM
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state     <= S_IDLE;
            busy      <= 1'b0;
            done      <= 1'b0;
            h_we      <= 1'b0;
            round_cnt <= 7'd0;
            wa <= 0; wb <= 0; wc <= 0; wd <= 0;
            we <= 0; wf <= 0; wg <= 0; wh <= 0;
            for (ki = 0; ki < 16; ki = ki + 1)
                W[ki] <= 32'd0;
            for (ki = 0; ki < 8; ki = ki + 1) begin
                H_save[ki] <= 32'd0;
                h_out[ki]  <= 32'd0;
            end
        end else begin
            done <= 1'b0;
            h_we <= 1'b0;

            case (state)
                S_IDLE: begin
                    if (start) begin
                        // Latch W and H from inputs
                        for (ki = 0; ki < 16; ki = ki + 1)
                            W[ki] <= w_in[ki];
                        for (ki = 0; ki < 8; ki = ki + 1)
                            H_save[ki] <= h_in[ki];
                        // Initialize working variables
                        wa <= h_in[0]; wb <= h_in[1];
                        wc <= h_in[2]; wd <= h_in[3];
                        we <= h_in[4]; wf <= h_in[5];
                        wg <= h_in[6]; wh <= h_in[7];
                        round_cnt <= 7'd0;
                        busy      <= 1'b1;
                        state     <= S_ROUND;
                    end
                end

                S_ROUND: begin
                    // Message schedule expansion for rounds >= 16
                    if (round_cnt >= 7'd16) begin
                        W[round_cnt[3:0]] <= w_new;
                    end

                    // Update working variables (one round)
                    wh <= wg;
                    wg <= wf;
                    wf <= we;
                    we <= wd + T1;
                    wd <= wc;
                    wc <= wb;
                    wb <= wa;
                    wa <= T1 + T2;

                    if (round_cnt == 7'd63) begin
                        state <= S_DONE;
                    end else begin
                        round_cnt <= round_cnt + 7'd1;
                    end
                end

                S_DONE: begin
                    // H' = H + working variables
                    h_out[0] <= H_save[0] + wa;
                    h_out[1] <= H_save[1] + wb;
                    h_out[2] <= H_save[2] + wc;
                    h_out[3] <= H_save[3] + wd;
                    h_out[4] <= H_save[4] + we;
                    h_out[5] <= H_save[5] + wf;
                    h_out[6] <= H_save[6] + wg;
                    h_out[7] <= H_save[7] + wh;
                    h_we  <= 1'b1;
                    done  <= 1'b1;
                    busy  <= 1'b0;
                    state <= S_IDLE;
                end

                default: state <= S_IDLE;
            endcase
        end
    end

endmodule
