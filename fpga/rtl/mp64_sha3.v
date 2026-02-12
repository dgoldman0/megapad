// ============================================================================
// mp64_sha3.v — SHA-3 / SHAKE Accelerator (Keccak-f[1600])
// ============================================================================
//
// MMIO base: 0x780 (64 bytes).
//
// Modes:
//   0 = SHA3-256 (rate=1088 bits = 136 bytes, output=256 bits)
//   1 = SHA3-512 (rate=576  bits = 72  bytes, output=512 bits)
//   2 = SHAKE128 (rate=1344 bits = 168 bytes, extendable)
//   3 = SHAKE256 (rate=1088 bits = 136 bytes, extendable)
//
// Keccak-f[1600]: 25 lanes × 64 bits = 1600-bit state
//   24 rounds, 1 round per cycle → 24 cycles per permutation
//
// Interface:
//   Write data words to DIN. After rate-many bytes, CMD=ABSORB triggers
//   Keccak-f permutation (~24 cycles). When done, STATUS.done=1.
//   CMD=SQUEEZE reads output via DOUT.
//   CMD=INIT resets state. CMD=FINAL pads + permutes.
//

`include "mp64_defs.vh"

module mp64_sha3 (
    input  wire        clk,
    input  wire        rst_n,

    // MMIO interface
    input  wire        req,
    input  wire [5:0]  addr,       // offset within SHA block
    input  wire [63:0] wdata,
    input  wire        wen,
    output reg  [63:0] rdata,
    output reg         ack,

    // Interrupt
    output reg         irq
);

    // ========================================================================
    // Keccak state: 5×5 array of 64-bit lanes = 1600 bits
    // ========================================================================
    reg [63:0] state [0:24];   // state[x + 5*y]

    // ========================================================================
    // Command/Status registers
    // ========================================================================
    localparam [2:0] CMD_NOP     = 3'd0;
    localparam [2:0] CMD_INIT    = 3'd1;
    localparam [2:0] CMD_ABSORB  = 3'd2;
    localparam [2:0] CMD_FINAL   = 3'd3;
    localparam [2:0] CMD_SQUEEZE = 3'd4;

    reg [1:0]  mode;           // 0=SHA3-256, 1=SHA3-512, 2=SHAKE128, 3=SHAKE256
    reg        busy;
    reg        done;
    reg [7:0]  rate_bytes;     // Rate in bytes for current mode
    reg [7:0]  din_ptr;        // Byte pointer into rate region

    // Data in/out buffers (64-bit access)
    reg [63:0] din_reg;
    reg [63:0] dout_reg;

    // ========================================================================
    // Keccak Round Constants
    // ========================================================================
    function [63:0] rc;
        input [4:0] round;
        begin
            case (round)
                5'd0:  rc = 64'h0000_0000_0000_0001;
                5'd1:  rc = 64'h0000_0000_0000_8082;
                5'd2:  rc = 64'h8000_0000_0000_808A;
                5'd3:  rc = 64'h8000_0000_8000_8000;
                5'd4:  rc = 64'h0000_0000_0000_808B;
                5'd5:  rc = 64'h0000_0000_8000_0001;
                5'd6:  rc = 64'h8000_0000_8000_8081;
                5'd7:  rc = 64'h8000_0000_0000_8009;
                5'd8:  rc = 64'h0000_0000_0000_008A;
                5'd9:  rc = 64'h0000_0000_0000_0088;
                5'd10: rc = 64'h0000_0000_8000_8009;
                5'd11: rc = 64'h0000_0000_8000_000A;
                5'd12: rc = 64'h0000_0000_8000_808B;
                5'd13: rc = 64'h8000_0000_0000_008B;
                5'd14: rc = 64'h8000_0000_0000_8089;
                5'd15: rc = 64'h8000_0000_0000_8003;
                5'd16: rc = 64'h8000_0000_0000_8002;
                5'd17: rc = 64'h8000_0000_0000_0080;
                5'd18: rc = 64'h0000_0000_0000_800A;
                5'd19: rc = 64'h8000_0000_8000_000A;
                5'd20: rc = 64'h8000_0000_8000_8081;
                5'd21: rc = 64'h8000_0000_0000_8080;
                5'd22: rc = 64'h0000_0000_8000_0001;
                5'd23: rc = 64'h8000_0000_8000_8008;
                default: rc = 64'd0;
            endcase
        end
    endfunction

    // Rotation offsets: rot[x + 5*y]
    function [5:0] rot_offset;
        input [4:0] idx;
        begin
            case (idx)
                5'd0:  rot_offset = 6'd0;   // (0,0)
                5'd1:  rot_offset = 6'd1;   // (1,0)
                5'd2:  rot_offset = 6'd62;  // (2,0)
                5'd3:  rot_offset = 6'd28;  // (3,0)
                5'd4:  rot_offset = 6'd27;  // (4,0)
                5'd5:  rot_offset = 6'd36;  // (0,1)
                5'd6:  rot_offset = 6'd44;  // (1,1)
                5'd7:  rot_offset = 6'd6;   // (2,1)
                5'd8:  rot_offset = 6'd55;  // (3,1)
                5'd9:  rot_offset = 6'd20;  // (4,1)
                5'd10: rot_offset = 6'd3;   // (0,2)
                5'd11: rot_offset = 6'd10;  // (1,2)
                5'd12: rot_offset = 6'd43;  // (2,2)
                5'd13: rot_offset = 6'd25;  // (3,2)
                5'd14: rot_offset = 6'd39;  // (4,2)
                5'd15: rot_offset = 6'd41;  // (0,3)
                5'd16: rot_offset = 6'd45;  // (1,3)
                5'd17: rot_offset = 6'd15;  // (2,3)
                5'd18: rot_offset = 6'd21;  // (3,3)
                5'd19: rot_offset = 6'd8;   // (4,3)
                5'd20: rot_offset = 6'd18;  // (0,4)
                5'd21: rot_offset = 6'd2;   // (1,4)
                5'd22: rot_offset = 6'd61;  // (2,4)
                5'd23: rot_offset = 6'd56;  // (3,4)
                5'd24: rot_offset = 6'd14;  // (4,4)
                default: rot_offset = 6'd0;
            endcase
        end
    endfunction

    // 64-bit rotate left
    function [63:0] rotl64;
        input [63:0] val;
        input [5:0]  amt;
        begin
            rotl64 = (val << amt) | (val >> (6'd0 - amt));
        end
    endfunction

    // ========================================================================
    // Keccak-f[1600] — one round combinational, 24 rounds sequential
    // ========================================================================
    reg [4:0]  round_cnt;
    reg        permuting;

    // Intermediate values for θ, ρ, π, χ, ι steps
    reg [63:0] c [0:4];        // Column parities
    reg [63:0] d [0:4];        // θ diffs
    reg [63:0] b [0:24];       // After θ+ρ+π
    reg [63:0] ns [0:24];      // Next state

    integer x, y, idx;

    // One Keccak round (combinational)
    always @(*) begin
        // θ step: compute column parities
        for (x = 0; x < 5; x = x + 1)
            c[x] = state[x] ^ state[x+5] ^ state[x+10] ^ state[x+15] ^ state[x+20];

        // θ step: compute D values
        for (x = 0; x < 5; x = x + 1)
            d[x] = c[(x+4)%5] ^ rotl64(c[(x+1)%5], 6'd1);

        // θ + ρ + π combined
        for (y = 0; y < 5; y = y + 1)
            for (x = 0; x < 5; x = x + 1) begin
                idx = x + 5*y;
                // π: B[y, 2x+3y mod 5] = rot(A[x,y] ^ D[x], r[x,y])
                b[y + 5*((2*x + 3*y) % 5)] = rotl64(state[idx] ^ d[x], rot_offset(idx[4:0]));
            end

        // χ step
        for (y = 0; y < 5; y = y + 1)
            for (x = 0; x < 5; x = x + 1)
                ns[x + 5*y] = b[x + 5*y] ^ (~b[((x+1)%5) + 5*y] & b[((x+2)%5) + 5*y]);

        // ι step (applied only to lane 0)
        ns[0] = ns[0] ^ rc(round_cnt);
    end

    // ========================================================================
    // Rate calculation
    // ========================================================================
    always @(*) begin
        case (mode)
            2'd0: rate_bytes = 8'd136;  // SHA3-256
            2'd1: rate_bytes = 8'd72;   // SHA3-512
            2'd2: rate_bytes = 8'd168;  // SHAKE128
            2'd3: rate_bytes = 8'd136;  // SHAKE256
        endcase
    end

    // ========================================================================
    // Main FSM
    // ========================================================================
    localparam [1:0] S_IDLE    = 2'd0;
    localparam [1:0] S_PERMUTE = 2'd1;
    localparam [1:0] S_PAD     = 2'd2;

    reg [1:0] sha_state;
    reg       do_final;    // Set when CMD_FINAL: pad first, then permute

    integer ki;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            sha_state  <= S_IDLE;
            busy       <= 1'b0;
            done       <= 1'b0;
            irq        <= 1'b0;
            permuting  <= 1'b0;
            round_cnt  <= 5'd0;
            mode       <= 2'd0;
            din_ptr    <= 8'd0;
            do_final   <= 1'b0;
            for (ki = 0; ki < 25; ki = ki + 1)
                state[ki] <= 64'd0;
        end else begin
            irq <= 1'b0;

            case (sha_state)
                S_IDLE: begin
                    done <= 1'b0;
                end

                S_PAD: begin
                    // Apply SHA-3 padding: byte at din_ptr gets 0x06 (SHA3) or 0x1F (SHAKE)
                    // Last byte of rate block gets 0x80 OR'd
                    // Then XOR into state
                    begin : pad_block
                        reg [7:0] pad_byte;
                        reg [4:0] lane_idx;
                        reg [2:0] byte_pos;
                        reg [63:0] pad_word;

                        // First pad byte
                        pad_byte = (mode >= 2'd2) ? 8'h1F : 8'h06;
                        lane_idx = din_ptr[7:3];
                        byte_pos = din_ptr[2:0];
                        pad_word = 64'd0;
                        pad_word[byte_pos*8 +: 8] = pad_byte;
                        state[lane_idx] <= state[lane_idx] ^ pad_word;

                        // Last byte of rate: OR 0x80
                        if (din_ptr == (rate_bytes - 8'd1)) begin
                            // Same byte gets both pad_byte and 0x80
                            pad_word[byte_pos*8 +: 8] = pad_byte | 8'h80;
                            state[lane_idx] <= state[lane_idx] ^ pad_word;
                        end else begin
                            // Set bit 7 of last rate byte
                            begin : last_byte_block
                                reg [7:0] last_ptr;
                                reg [4:0] last_lane;
                                reg [2:0] last_bpos;
                                reg [63:0] last_word;
                                last_ptr = rate_bytes - 8'd1;
                                last_lane = last_ptr[7:3];
                                last_bpos = last_ptr[2:0];
                                last_word = 64'd0;
                                last_word[last_bpos*8 +: 8] = 8'h80;
                                state[last_lane] <= state[last_lane] ^ last_word;
                            end
                        end
                    end

                    // Start permutation
                    round_cnt <= 5'd0;
                    permuting <= 1'b1;
                    sha_state <= S_PERMUTE;
                end

                S_PERMUTE: begin
                    if (permuting) begin
                        // Apply one round
                        for (ki = 0; ki < 25; ki = ki + 1)
                            state[ki] <= ns[ki];

                        if (round_cnt == 5'd23) begin
                            permuting <= 1'b0;
                            busy <= 1'b0;
                            done <= 1'b1;
                            irq <= 1'b1;
                            din_ptr <= 8'd0;
                            sha_state <= S_IDLE;
                        end else begin
                            round_cnt <= round_cnt + 5'd1;
                        end
                    end
                end

                default: sha_state <= S_IDLE;
            endcase

            // ============================================================
            // Register writes from MMIO
            // ============================================================
            if (req && wen) begin
                case (addr)
                    // CMD register
                    6'h00: begin
                        case (wdata[2:0])
                            CMD_INIT: begin
                                // Reset state
                                for (ki = 0; ki < 25; ki = ki + 1)
                                    state[ki] <= 64'd0;
                                din_ptr <= 8'd0;
                                done <= 1'b0;
                                busy <= 1'b0;
                            end
                            CMD_ABSORB: begin
                                // Start Keccak-f permutation
                                busy <= 1'b1;
                                done <= 1'b0;
                                round_cnt <= 5'd0;
                                permuting <= 1'b1;
                                sha_state <= S_PERMUTE;
                            end
                            CMD_FINAL: begin
                                // Pad then permute
                                busy <= 1'b1;
                                done <= 1'b0;
                                do_final <= 1'b1;
                                sha_state <= S_PAD;
                            end
                            CMD_SQUEEZE: begin
                                // Additional squeeze: just re-permute
                                busy <= 1'b1;
                                done <= 1'b0;
                                round_cnt <= 5'd0;
                                permuting <= 1'b1;
                                sha_state <= S_PERMUTE;
                            end
                            default: ;
                        endcase
                    end

                    // DIN register — XOR data into state at rate position
                    6'h10: begin
                        if (din_ptr < rate_bytes) begin : din_write_block
                            reg [4:0] lane;
                            lane = din_ptr[7:3];
                            state[lane] <= state[lane] ^ wdata;
                            din_ptr <= din_ptr + 8'd8;
                        end
                    end

                    // RATE register (override)
                    6'h20: ; // Read-only, derived from mode

                    // CTRL register (mode select)
                    6'h28: begin
                        mode <= wdata[1:0];
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
                    6'h08: rdata <= {62'd0, done, busy};  // STATUS
                    // DOUT — read state lanes sequentially
                    6'h18: begin
                        if (din_ptr < rate_bytes) begin
                            rdata <= state[din_ptr[7:3]];
                            din_ptr <= din_ptr + 8'd8;
                        end else begin
                            rdata <= 64'd0;
                        end
                    end
                    6'h20: rdata <= {56'd0, rate_bytes};   // RATE
                    6'h28: rdata <= {62'd0, mode};         // CTRL
                    default: rdata <= 64'd0;
                endcase
            end else begin
                rdata <= 64'd0;
            end
        end
    end

endmodule
