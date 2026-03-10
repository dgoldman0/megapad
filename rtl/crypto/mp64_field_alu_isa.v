// ============================================================================
// mp64_field_alu_isa.v — Per-Core Field ALU ISA Engine
// ============================================================================
//
// Per-core field ALU compute engine for EXT.CRYPTO (FB 2x) ISA instructions.
// Instantiated per core.  Operates on 256-bit ACC registers and 256-bit tile
// memory operand (M[TSRC0]).  Replaces shared MMIO mp64_field_alu.v for
// per-core ISA dispatch.
//
// Sub-ops (op[3:0]):
//   0x0 GF.ADD     — (ACC + B) mod p                    1 cycle
//   0x1 GF.SUB     — (ACC − B) mod p                    1 cycle
//   0x2 GF.MUL     — (ACC × B) mod p                    1–4 cycles
//   0x3 GF.SQR     — ACC² mod p                         1–4 cycles
//   0x4 GF.INV     — ACC^(p−2) mod p                    ~767 cycles
//   0x5 GF.POW     — ACC^B mod p                        ~767 cycles
//   0x6 GF.MULR    — raw 256×256→512: {M[TDST],ACC}     1 cycle
//   0x7 GF.MAC     — (ACC×B + prev) mod p               1–4 cycles
//   0x8 GF.MACR    — prev_512 + ACC×B (raw)             1 cycle
//   0x9 GF.CMOV Rd — cond move: if R[d]!=0, ACC←B       1 cycle
//   0xA GF.CEQ     — const-time eq: ACC←(ACC==B)?1:0    1 cycle
//   0xB GF.PRIME   — select prime (imm8)                1 cycle
//   0xC GF.LDPRIME — load custom prime from ACC+B       1 cycle
//   0xD GF.X25519  — full scalar multiply               ~4335 cycles
//
// State:
//   - Operand A:   ACC0–ACC3 (256-bit, from CPU CSRs)
//   - Operand B:   256-bit tile_b input (from M[TSRC0])
//   - Result:      acc_out (→ ACC0–ACC3), tile_dst_out (→ M[TDST])
//   - Persistent:  prev_lo/prev_hi (for MAC/MACR accumulate)
//
// Multi-prime: prime_sel input (from CSR 0x85) selects modulus.
//   0 = Curve25519  (2^255 − 19)
//   1 = secp256k1   (2^256 − 2^32 − 977)
//   2 = P-256       (NIST)
//   3 = Custom      (via GF.LDPRIME)
//

`include "mp64_pkg.vh"

module mp64_field_alu_isa (
    input  wire        clk,
    input  wire        rst_n,

    // -- Command interface --
    input  wire        start,          // pulse: begin operation
    input  wire [3:0]  op,             // sub-op 0x0–0xD

    // -- Register operands --
    input  wire [63:0] rd_val,         // R[Rd] for GF.CMOV condition
    input  wire [7:0]  imm8,           // immediate byte for GF.PRIME

    // -- 256-bit ACC input (packed: ACC0 at [63:0], ACC3 at [255:192]) --
    input  wire [255:0] acc_in,

    // -- 256-bit B operand (M[TSRC0], 32 bytes little-endian) --
    input  wire [255:0] tile_b,

    // -- Prime configuration (from CSRs / CPU state) --
    input  wire [1:0]  prime_sel,
    input  wire [255:0] custom_p,
    input  wire [255:0] mont_pinv,

    // -- Results --
    output reg  [255:0] acc_out,       // new ACC (written when acc_we=1)
    output reg          acc_we,

    output reg  [255:0] tile_dst_out,  // high half for MULR/MACR (→ M[TDST])
    output reg          tile_dst_we,

    output reg  [1:0]   prime_sel_out, // for GF.PRIME
    output reg          prime_sel_we,

    output reg  [255:0] ldprime_p_out, // for GF.LDPRIME: custom prime
    output reg  [255:0] ldprime_pinv_out, // for GF.LDPRIME: Montgomery pinv
    output reg          ldprime_we,

    output reg          flag_z,        // Z flag for GF.CEQ
    output reg          flag_z_we,

    // -- Status --
    output reg          busy,
    output reg          done
);

    // ========================================================================
    // Constants
    // ========================================================================
    localparam [255:0] PRIME_25519  = 256'h7FFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFED;
    localparam [255:0] PRIME_SECP   = 256'hFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFE_FFFFFC2F;
    localparam [255:0] PRIME_P256   = 256'hFFFFFFFF_00000001_00000000_00000000_00000000_FFFFFFFF_FFFFFFFF_FFFFFFFF;
    localparam [255:0] A24          = 256'd121665;

    localparam [255:0] P_MINUS_2_25519 = 256'h7FFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFEB;
    localparam [255:0] P_MINUS_2_SECP  = 256'hFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFE_FFFFFC2D;
    localparam [255:0] P_MINUS_2_P256  = 256'hFFFFFFFF_00000001_00000000_00000000_00000000_FFFFFFFF_FFFFFFFF_FFFFFFFD;

    // Op encoding
    localparam [3:0] OP_ADD     = 4'h0,
                     OP_SUB     = 4'h1,
                     OP_MUL     = 4'h2,
                     OP_SQR     = 4'h3,
                     OP_INV     = 4'h4,
                     OP_POW     = 4'h5,
                     OP_MULR    = 4'h6,
                     OP_MAC     = 4'h7,
                     OP_MACR    = 4'h8,
                     OP_CMOV    = 4'h9,
                     OP_CEQ     = 4'hA,
                     OP_PRIME   = 4'hB,
                     OP_LDPRIME = 4'hC,
                     OP_X25519  = 4'hD;

    // FSM states
    localparam [3:0] S_IDLE       = 4'd0,
                     S_COMPUTE    = 4'd1,
                     S_POWER      = 4'd2,   // shared: INV / POW / X25519 invert
                     S_REDC       = 4'd3,   // Montgomery REDC pipeline
                     S_X_CLAMP    = 4'd4,   // X25519: clamp + init
                     S_X_LADDER   = 4'd5,   // X25519: Montgomery ladder
                     S_X_FSWAP    = 4'd6,   // X25519: final cswap
                     S_X_FMUL     = 4'd7,   // X25519: final mul
                     S_DONE       = 4'd8;

    reg [3:0]   state;
    reg [3:0]   op_reg;

    // ========================================================================
    // Persistent accumulate state (survives across calls)
    // ========================================================================
    reg [255:0] prev_lo;
    reg [255:0] prev_hi;

    // ========================================================================
    // Exponentiation state (INV, POW, X25519)
    // ========================================================================
    reg [255:0] pow_acc;
    reg [255:0] pow_base;
    reg [255:0] pow_exp;
    reg [8:0]   pow_step;
    reg [1:0]   pow_phase;

    // ========================================================================
    // Montgomery REDC state
    // ========================================================================
    reg [1:0]   redc_phase;
    reg [511:0] redc_T;
    reg [255:0] redc_m;
    reg [511:0] redc_mp;
    reg         redc_is_mac;
    reg [255:0] redc_save;     // saved prev_lo for MAC accumulate

    // ========================================================================
    // X25519 ladder state
    // ========================================================================
    reg [255:0] x_scalar;
    reg [255:0] x_ucoord;
    reg [255:0] X2, Z2, X3, Z3;
    reg         swap_bit;
    reg [7:0]   bit_idx;
    reg [3:0]   ladder_phase;

    // Ladder temporaries
    reg [255:0] rA, rB, rAA, rBB, rE, rC, rD, rDA, rCB, rT;

    // ========================================================================
    // Field arithmetic functions (identical to mp64_field_alu.v)
    // ========================================================================

    function [255:0] field_add;
        input [255:0] a, b;
        reg [256:0] sum;
        begin
            sum = {1'b0, a} + {1'b0, b};
            if (sum >= {1'b0, PRIME_25519})
                field_add = sum - {1'b0, PRIME_25519};
            else
                field_add = sum[255:0];
        end
    endfunction

    function [255:0] field_sub;
        input [255:0] a, b;
        begin
            if (a >= b)
                field_sub = a - b;
            else
                field_sub = PRIME_25519 - (b - a);
        end
    endfunction

    function [255:0] field_reduce;
        input [511:0] x;
        reg [255:0] lo;
        reg [256:0] hi;
        reg [511:0] partial;
        reg [256:0] r;
        begin
            lo = x[254:0];
            hi = x[511:255];
            partial = {1'b0, lo} + hi * 19;
            lo = partial[254:0];
            hi = partial[511:255];
            partial = {1'b0, lo} + hi * 19;
            r = partial[256:0];
            if (r >= {1'b0, PRIME_25519}) r = r - {1'b0, PRIME_25519};
            if (r >= {1'b0, PRIME_25519}) r = r - {1'b0, PRIME_25519};
            field_reduce = r[255:0];
        end
    endfunction

    function [255:0] field_mul;
        input [255:0] a, b;
        begin
            field_mul = field_reduce(a * b);
        end
    endfunction

    function [255:0] clamp;
        input [255:0] s;
        reg [255:0] r;
        begin
            r       = s;
            r[0]    = 1'b0;
            r[1]    = 1'b0;
            r[2]    = 1'b0;
            r[255]  = 1'b0;
            r[254]  = 1'b1;
            clamp   = r;
        end
    endfunction

    // secp256k1 reduction
    function [255:0] field_reduce_secp;
        input [511:0] x;
        reg [289:0] hi_times_c;
        reg [289:0] fold1;
        reg [256:0] ov_times_c;
        reg [256:0] fold2;
        begin
            hi_times_c = x[511:256] * 290'd4294968273;
            fold1 = {34'd0, x[255:0]} + hi_times_c;
            ov_times_c = fold1[289:256] * 257'd4294968273;
            fold2 = {1'b0, fold1[255:0]} + ov_times_c;
            if (fold2 >= {1'b0, PRIME_SECP})
                fold2 = fold2 - {1'b0, PRIME_SECP};
            field_reduce_secp = fold2[255:0];
        end
    endfunction

    // NIST P-256 reduction
    function [255:0] field_reduce_p256;
        input [511:0] x;
        reg [31:0] c [0:15];
        reg [255:0] s1, s2, s3, s4, s5, d1, d2, d3, d4;
        reg [263:0] pos, neg;
        reg [264:0] adj;
        integer i;
        begin
            for (i = 0; i < 16; i = i + 1)
                c[i] = x[i*32 +: 32];
            s1 = {c[7],  c[6],  c[5],  c[4],  c[3],  c[2],  c[1],  c[0]};
            s2 = {c[15], c[14], c[13], c[12], c[11], 32'd0, 32'd0, 32'd0};
            s3 = {32'd0, c[15], c[14], c[13], c[12], 32'd0, 32'd0, 32'd0};
            s4 = {c[15], c[14], 32'd0, 32'd0, 32'd0, c[10], c[9],  c[8]};
            s5 = {c[8],  c[13], c[15], c[14], c[13], c[11], c[10], c[9]};
            d1 = {c[10], c[8],  32'd0, 32'd0, 32'd0, c[13], c[12], c[11]};
            d2 = {c[11], c[9],  32'd0, 32'd0, c[15], c[14], c[13], c[12]};
            d3 = {c[12], 32'd0, c[10], c[9],  c[8],  c[15], c[14], c[13]};
            d4 = {c[13], 32'd0, c[11], c[10], c[9],  32'd0, c[15], c[14]};
            pos = s1 + s2 + s2 + s3 + s3 + s4 + s5;
            neg = d1 + d2 + d3 + d4;
            adj = {1'b0, pos}
                + {9'd0, PRIME_P256} + {9'd0, PRIME_P256}
                + {9'd0, PRIME_P256} + {9'd0, PRIME_P256}
                + {9'd0, PRIME_P256}
                - {1'b0, neg};
            for (i = 0; i < 16; i = i + 1)
                if (adj >= {9'd0, PRIME_P256})
                    adj = adj - {9'd0, PRIME_P256};
            field_reduce_p256 = adj[255:0];
        end
    endfunction

    // Prime-parameterized dispatch
    function [255:0] field_add_sel;
        input [255:0] a, b;
        input [1:0] sel;
        input [255:0] cp;
        reg [256:0] sum;
        reg [255:0] p;
        begin
            case (sel)
                2'd0:    p = PRIME_25519;
                2'd1:    p = PRIME_SECP;
                2'd2:    p = PRIME_P256;
                2'd3:    p = cp;
            endcase
            sum = {1'b0, a} + {1'b0, b};
            if (sum >= {1'b0, p})
                field_add_sel = sum - {1'b0, p};
            else
                field_add_sel = sum[255:0];
        end
    endfunction

    function [255:0] field_sub_sel;
        input [255:0] a, b;
        input [1:0] sel;
        input [255:0] cp;
        reg [255:0] p;
        begin
            case (sel)
                2'd0:    p = PRIME_25519;
                2'd1:    p = PRIME_SECP;
                2'd2:    p = PRIME_P256;
                2'd3:    p = cp;
            endcase
            if (a >= b)
                field_sub_sel = a - b;
            else
                field_sub_sel = p - (b - a);
        end
    endfunction

    function [255:0] field_mul_sel;
        input [255:0] a, b;
        input [1:0] sel;
        input [255:0] cp;
        reg [511:0] prod;
        begin
            prod = a * b;
            case (sel)
                2'd0:    field_mul_sel = field_reduce(prod);
                2'd1:    field_mul_sel = field_reduce_secp(prod);
                2'd2:    field_mul_sel = field_reduce_p256(prod);
                2'd3:    field_mul_sel = prod % {256'd0, cp};
            endcase
        end
    endfunction

    // Effective prime selection: X25519 always uses Curve25519 prime
    wire [1:0] eff_prime = (op_reg == OP_X25519) ? 2'd0 : prime_sel;

    // ========================================================================
    // Main FSM
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state        <= S_IDLE;
            op_reg       <= 4'd0;
            busy         <= 1'b0;
            done         <= 1'b0;
            acc_out      <= 256'd0;
            acc_we       <= 1'b0;
            tile_dst_out <= 256'd0;
            tile_dst_we  <= 1'b0;
            prime_sel_out <= 2'd0;
            prime_sel_we <= 1'b0;
            ldprime_p_out    <= 256'd0;
            ldprime_pinv_out <= 256'd0;
            ldprime_we   <= 1'b0;
            flag_z       <= 1'b0;
            flag_z_we    <= 1'b0;
            prev_lo      <= 256'd0;
            prev_hi      <= 256'd0;
            pow_acc      <= 256'd0;
            pow_base     <= 256'd0;
            pow_exp      <= 256'd0;
            pow_step     <= 9'd0;
            pow_phase    <= 2'd0;
            redc_phase   <= 2'd0;
            redc_T       <= 512'd0;
            redc_m       <= 256'd0;
            redc_mp      <= 512'd0;
            redc_is_mac  <= 1'b0;
            redc_save    <= 256'd0;
            x_scalar     <= 256'd0;
            x_ucoord     <= 256'd0;
            X2 <= 0; Z2 <= 0; X3 <= 0; Z3 <= 0;
            swap_bit     <= 1'b0;
            bit_idx      <= 8'd0;
            ladder_phase <= 4'd0;
            rA <= 0; rB <= 0; rAA <= 0; rBB <= 0; rE <= 0;
            rC <= 0; rD <= 0; rDA <= 0; rCB <= 0; rT <= 0;
        end else begin

            // Default: deassert one-shot outputs
            done         <= 1'b0;
            acc_we       <= 1'b0;
            tile_dst_we  <= 1'b0;
            prime_sel_we <= 1'b0;
            ldprime_we   <= 1'b0;
            flag_z_we    <= 1'b0;

            case (state)

                // ============================================================
                // S_IDLE — wait for start pulse
                // ============================================================
                S_IDLE: begin
                    if (start) begin
                        op_reg <= op;
                        busy   <= 1'b1;

                        case (op)
                            // Single-cycle compute ops
                            OP_ADD, OP_SUB, OP_MULR, OP_MACR,
                            OP_CMOV, OP_CEQ, OP_LDPRIME:
                                state <= S_COMPUTE;

                            // GF.PRIME — immediate, no compute
                            OP_PRIME: begin
                                prime_sel_out <= imm8[1:0];
                                prime_sel_we  <= 1'b1;
                                busy <= 1'b0;
                                done <= 1'b1;
                                // stay in S_IDLE
                            end

                            // MUL/SQR — may need REDC for custom prime
                            OP_MUL: begin
                                if (prime_sel == 2'd3 && mont_pinv != 256'd0) begin
                                    redc_T       <= acc_in * tile_b;
                                    redc_phase   <= 2'd0;
                                    redc_is_mac  <= 1'b0;
                                    state        <= S_REDC;
                                end else
                                    state <= S_COMPUTE;
                            end
                            OP_SQR: begin
                                if (prime_sel == 2'd3 && mont_pinv != 256'd0) begin
                                    redc_T       <= acc_in * acc_in;
                                    redc_phase   <= 2'd0;
                                    redc_is_mac  <= 1'b0;
                                    state        <= S_REDC;
                                end else
                                    state <= S_COMPUTE;
                            end
                            OP_MAC: begin
                                if (prime_sel == 2'd3 && mont_pinv != 256'd0) begin
                                    redc_T       <= acc_in * tile_b;
                                    redc_phase   <= 2'd0;
                                    redc_is_mac  <= 1'b1;
                                    redc_save    <= prev_lo;
                                    state        <= S_REDC;
                                end else
                                    state <= S_COMPUTE;
                            end

                            // Multi-cycle exponentiation
                            OP_INV: begin
                                pow_acc   <= 256'd1;
                                pow_base  <= acc_in;
                                case (prime_sel)
                                    2'd1:    pow_exp <= P_MINUS_2_SECP;
                                    2'd2:    pow_exp <= P_MINUS_2_P256;
                                    2'd3:    pow_exp <= custom_p - 256'd2;
                                    default: pow_exp <= P_MINUS_2_25519;
                                endcase
                                pow_step  <= 9'd0;
                                pow_phase <= 2'd0;
                                state     <= S_POWER;
                            end
                            OP_POW: begin
                                pow_acc   <= 256'd1;
                                pow_base  <= acc_in;
                                pow_exp   <= tile_b;
                                pow_step  <= 9'd0;
                                pow_phase <= 2'd0;
                                state     <= S_POWER;
                            end

                            // X25519 scalar multiplication
                            OP_X25519: begin
                                x_scalar <= acc_in;
                                x_ucoord <= tile_b;
                                state    <= S_X_CLAMP;
                            end

                            default: begin
                                busy <= 1'b0;
                                done <= 1'b1;
                            end
                        endcase
                    end
                end

                // ============================================================
                // S_COMPUTE — single-cycle operations
                // ============================================================
                S_COMPUTE: begin
                    case (op_reg)
                        OP_ADD: begin
                            acc_out <= field_add_sel(acc_in, tile_b, prime_sel, custom_p);
                            acc_we  <= 1'b1;
                            prev_lo <= field_add_sel(acc_in, tile_b, prime_sel, custom_p);
                        end
                        OP_SUB: begin
                            acc_out <= field_sub_sel(acc_in, tile_b, prime_sel, custom_p);
                            acc_we  <= 1'b1;
                            prev_lo <= field_sub_sel(acc_in, tile_b, prime_sel, custom_p);
                        end
                        OP_MUL: begin
                            acc_out <= field_mul_sel(acc_in, tile_b, prime_sel, custom_p);
                            acc_we  <= 1'b1;
                            prev_lo <= field_mul_sel(acc_in, tile_b, prime_sel, custom_p);
                        end
                        OP_SQR: begin
                            acc_out <= field_mul_sel(acc_in, acc_in, prime_sel, custom_p);
                            acc_we  <= 1'b1;
                            prev_lo <= field_mul_sel(acc_in, acc_in, prime_sel, custom_p);
                        end
                        OP_MULR: begin
                            acc_out      <= (acc_in * tile_b);
                            tile_dst_out <= (acc_in * tile_b) >> 256;
                            acc_we       <= 1'b1;
                            tile_dst_we  <= 1'b1;
                            prev_lo      <= (acc_in * tile_b);
                            prev_hi      <= (acc_in * tile_b) >> 256;
                        end
                        OP_MAC: begin
                            // (ACC × B + prev) mod p
                            acc_out <= field_add_sel(
                                field_mul_sel(acc_in, tile_b, prime_sel, custom_p),
                                prev_lo, prime_sel, custom_p);
                            acc_we  <= 1'b1;
                            prev_lo <= field_add_sel(
                                field_mul_sel(acc_in, tile_b, prime_sel, custom_p),
                                prev_lo, prime_sel, custom_p);
                        end
                        OP_MACR: begin
                            // Raw: prev_512 + ACC × B
                            begin : macr_calc
                                reg [511:0] prod;
                                reg [511:0] prev_512;
                                reg [511:0] total;
                                prod     = acc_in * tile_b;
                                prev_512 = {prev_hi, prev_lo};
                                total    = prev_512 + prod;
                                acc_out      <= total[255:0];
                                tile_dst_out <= total[511:256];
                                acc_we       <= 1'b1;
                                tile_dst_we  <= 1'b1;
                                prev_lo      <= total[255:0];
                                prev_hi      <= total[511:256];
                            end
                        end
                        OP_CMOV: begin
                            // Constant-time conditional move
                            if (rd_val != 64'd0) begin
                                acc_out <= tile_b;
                                acc_we  <= 1'b1;
                                prev_lo <= tile_b;
                            end
                            // else: no write (ACC unchanged)
                        end
                        OP_CEQ: begin
                            // Constant-time equality
                            acc_out   <= (|(acc_in ^ tile_b)) ? 256'd0 : 256'd1;
                            acc_we    <= 1'b1;
                            flag_z    <= ~(|(acc_in ^ tile_b));
                            flag_z_we <= 1'b1;
                            prev_lo   <= (|(acc_in ^ tile_b)) ? 256'd0 : 256'd1;
                        end
                        OP_LDPRIME: begin
                            ldprime_p_out    <= acc_in;
                            ldprime_pinv_out <= tile_b;
                            ldprime_we       <= 1'b1;
                        end
                        default: ;  // shouldn't reach here
                    endcase
                    busy  <= 1'b0;
                    done  <= 1'b1;
                    state <= S_IDLE;
                end

                // ============================================================
                // S_POWER — binary square-and-multiply exponentiation
                // Used by GF.INV, GF.POW, and X25519 (Z2 inversion).
                // ============================================================
                S_POWER: begin
                    case (pow_phase)
                        2'd0: begin
                            pow_acc <= field_mul_sel(pow_acc, pow_acc, eff_prime, custom_p);
                            pow_phase <= 2'd1;
                        end
                        2'd1: begin
                            if (pow_exp[255 - pow_step[7:0]])
                                pow_acc <= field_mul_sel(pow_acc, pow_base, eff_prime, custom_p);
                            pow_phase <= 2'd2;
                        end
                        2'd2: begin
                            if (pow_step == 9'd255) begin
                                if (op_reg == OP_X25519) begin
                                    // X25519: result = X2 * Z2_inv
                                    state <= S_X_FMUL;
                                end else begin
                                    acc_out <= pow_acc;
                                    acc_we  <= 1'b1;
                                    prev_lo <= pow_acc;
                                    busy    <= 1'b0;
                                    done    <= 1'b1;
                                    state   <= S_IDLE;
                                end
                            end else begin
                                pow_step  <= pow_step + 9'd1;
                                pow_phase <= 2'd0;
                            end
                        end
                        default: pow_phase <= 2'd0;
                    endcase
                end

                // ============================================================
                // S_REDC — Montgomery REDC pipeline (prime_sel=3)
                // ============================================================
                S_REDC: begin
                    case (redc_phase)
                        2'd0: begin
                            redc_m <= (redc_T[255:0] * mont_pinv);
                            redc_phase <= 2'd1;
                        end
                        2'd1: begin
                            redc_mp <= redc_m * custom_p;
                            redc_phase <= 2'd2;
                        end
                        2'd2: begin
                            begin : redc_final
                                reg [512:0] sum;
                                reg [256:0] t;
                                sum = {1'b0, redc_T} + {1'b0, redc_mp};
                                t   = sum[512:256];
                                if (t >= {1'b0, custom_p})
                                    t = t - {1'b0, custom_p};
                                acc_out <= t[255:0];
                                acc_we  <= 1'b1;
                                prev_lo <= t[255:0];
                            end
                            if (redc_is_mac)
                                redc_phase <= 2'd3;
                            else begin
                                busy  <= 1'b0;
                                done  <= 1'b1;
                                state <= S_IDLE;
                            end
                        end
                        2'd3: begin
                            // FMAC accumulate: add saved prev_lo to REDC result
                            acc_out <= field_add_sel(acc_out, redc_save,
                                                    prime_sel, custom_p);
                            acc_we  <= 1'b1;
                            prev_lo <= field_add_sel(acc_out, redc_save,
                                                    prime_sel, custom_p);
                            busy    <= 1'b0;
                            done    <= 1'b1;
                            state   <= S_IDLE;
                        end
                    endcase
                end

                // ============================================================
                // S_X_CLAMP — X25519: clamp scalar + init ladder
                // ============================================================
                S_X_CLAMP: begin
                    x_scalar         <= clamp(x_scalar);
                    x_ucoord[255]    <= 1'b0;
                    X2               <= 256'd1;
                    Z2               <= 256'd0;
                    X3               <= x_ucoord;
                    X3[255]          <= 1'b0;
                    Z3               <= 256'd1;
                    bit_idx          <= 8'd254;
                    swap_bit         <= 1'b0;
                    ladder_phase     <= 4'd0;
                    state            <= S_X_LADDER;
                end

                // ============================================================
                // S_X_LADDER — Montgomery ladder (RFC 7748 §5)
                // ============================================================
                S_X_LADDER: begin
                    case (ladder_phase)
                        4'd0: begin
                            if (swap_bit ^ x_scalar[bit_idx]) begin
                                X2 <= X3;  Z2 <= Z3;
                                X3 <= X2;  Z3 <= Z2;
                                rA <= field_add(X3, Z3);
                                rB <= field_sub(X3, Z3);
                            end else begin
                                rA <= field_add(X2, Z2);
                                rB <= field_sub(X2, Z2);
                            end
                            swap_bit <= x_scalar[bit_idx];
                            ladder_phase <= 4'd1;
                        end
                        4'd1:  begin rAA <= field_mul(rA, rA);  ladder_phase <= 4'd2;  end
                        4'd2:  begin rBB <= field_mul(rB, rB);  ladder_phase <= 4'd3;  end
                        4'd3: begin
                            rE <= field_sub(rAA, rBB);
                            rC <= field_add(X3, Z3);
                            rD <= field_sub(X3, Z3);
                            ladder_phase <= 4'd4;
                        end
                        4'd4:  begin rDA <= field_mul(rD, rA);  ladder_phase <= 4'd5;  end
                        4'd5:  begin rCB <= field_mul(rC, rB);  ladder_phase <= 4'd6;  end
                        4'd6:  begin X2  <= field_mul(rAA, rBB); ladder_phase <= 4'd7;  end
                        4'd7:  begin rT  <= field_mul(A24, rE);  ladder_phase <= 4'd8;  end
                        4'd8: begin
                            rT  <= field_add(rAA, rT);
                            rDA <= field_add(rDA, rCB);
                            rCB <= field_sub(rDA, rCB);
                            ladder_phase <= 4'd9;
                        end
                        4'd9:  begin X3  <= field_mul(rDA, rDA);   ladder_phase <= 4'd10; end
                        4'd10: begin rCB <= field_mul(rCB, rCB);   ladder_phase <= 4'd11; end
                        4'd11: begin Z3  <= field_mul(x_ucoord, rCB); ladder_phase <= 4'd12; end
                        4'd12: begin Z2  <= field_mul(rE, rT);     ladder_phase <= 4'd13; end
                        4'd13: begin
                            if (bit_idx == 8'd0)
                                state <= S_X_FSWAP;
                            else begin
                                bit_idx <= bit_idx - 8'd1;
                                ladder_phase <= 4'd0;
                            end
                        end
                        default: ladder_phase <= 4'd0;
                    endcase
                end

                // ============================================================
                // S_X_FSWAP — final conditional swap + start inversion
                // ============================================================
                S_X_FSWAP: begin
                    if (swap_bit) begin
                        X2 <= X3;  Z2 <= Z3;
                        X3 <= X2;  Z3 <= Z2;
                        pow_base <= Z3;
                    end else begin
                        pow_base <= Z2;
                    end
                    pow_acc   <= 256'd1;
                    pow_exp   <= P_MINUS_2_25519;
                    pow_step  <= 9'd0;
                    pow_phase <= 2'd0;
                    state     <= S_POWER;
                end

                // ============================================================
                // S_X_FMUL — X25519 final: result = X2 * Z2_inv
                // ============================================================
                S_X_FMUL: begin
                    acc_out <= field_mul(X2, pow_acc);
                    acc_we  <= 1'b1;
                    prev_lo <= field_mul(X2, pow_acc);
                    busy    <= 1'b0;
                    done    <= 1'b1;
                    state   <= S_IDLE;
                end

                // ============================================================
                // S_DONE — not used (ops return directly to S_IDLE)
                // ============================================================
                S_DONE: begin
                    state <= S_IDLE;
                end

                default: state <= S_IDLE;

            endcase
        end
    end

endmodule
