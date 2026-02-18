// ============================================================================
// mp64_field_alu.v — Multi-Prime Field ALU + Raw Multiplier
// ============================================================================
//
// Supersedes mp64_x25519.v.  Mode 0 is backward-compatible X25519 scalar
// multiplication (RFC 7748).  Modes 1–7 expose field primitives and a raw
// 256×256→512-bit multiplier.  Modes 8–12 add constant-time ops, Montgomery
// loading, and multiply-accumulate.
//
// Multi-prime support: prime_sel[1:0] (CMD bits [7:6]) selects the
// modulus for field operations.  X25519 (mode 0) always uses Curve25519.
//   0 = Curve25519  (2^255 - 19)        — fast ×38 fold reducer
//   1 = secp256k1   (2^256 - 2^32 - 977) — ×(2^32+977) fold reducer
//   2 = P-256       (NIST)               — NIST special form (reserved)
//   3 = Custom      (Montgomery REDC)    — generic (reserved)
//
// Target: Kintex-7 / Artix-7.
// Resource estimate:
//   ~165 DSP48E1 slices  (shared 256×256 multiplier, operand-muxed)
//   ~5400 flip-flops     (state + temporaries + prime regs)
//   ~4500 LUTs           (field_add/sub + reducers)
//
// Cycle counts per mode:
//   0  X25519     ≈ 4335  (255 ladder + 255×3 inversion + overhead)
//   1  FADD(a+b)  1
//   2  FSUB(a−b)  1
//   3  FMUL(a·b)  1
//   4  FSQR(a²)   1
//   5  FINV       ≈ 767   (Fermat a^(p−2))
//   6  FPOW       ≈ 767   (a^b mod p, binary method)
//   7  MUL_RAW    1       (256×256→512 no reduction)
//   8  FCMOV      1       (constant-time conditional move) [reserved]
//   9  FCEQ       1       (constant-time equality test)    [reserved]
//  10  LOAD_PRIME 1       (latch custom prime + p_inv)     [reserved]
//  11  FMAC       1       (field multiply-accumulate)      [reserved]
//  12  MUL_ADD_RAW 1      (raw multiply-accumulate)        [reserved]
//
// MMIO base: 0x840 (64-byte block, addr[5:0]).
//
// Register map (byte offsets from base):
//   ── Write (64-bit) ─────────────────────────────────────────────
//   0x00  OPERAND_A[ 63:  0]     scalar (mode 0) / operand a
//   0x08  OPERAND_A[127: 64]
//   0x10  OPERAND_A[191:128]
//   0x18  OPERAND_A[255:192]
//   0x20  OPERAND_B[ 63:  0]     u-coord (mode 0) / operand b
//   0x28  OPERAND_B[127: 64]
//   0x30  OPERAND_B[191:128]
//   0x38  OPERAND_B[255:192]     (addr[2:0]==0)
//   0x3F  CMD                    (addr[2:0]!=0)
//         wdata[0]   = go        1 = start computation
//         wdata[4:1] = mode      0–12, see above
//         wdata[5]   = result_sel  0=LO half, 1=HI half (MUL_RAW)
//         wdata[7:6] = prime_sel   0–3, latched on go=0 writes only
//
//   ── Read (64-bit) ──────────────────────────────────────────────
//   0x00  STATUS   {62'd0, done, busy}
//   0x08  RESULT[ 63:  0]   — selected by result_sel
//   0x10  RESULT[127: 64]
//   0x18  RESULT[191:128]
//   0x20  RESULT[255:192]
//   0x28–0x38  reserved (reads 0)
//
//   result_sel defaults to 0 (LO) after every operation.
//   To read RESULT_HI after MUL_RAW: write CMD with go=0, result_sel=1,
//   then read 0x08–0x20 again.
//
//   prime_sel is latched only on CMD writes with go=0 (backward compatible:
//   old code writes go=1 with bits [7:6]=0, which does not change prime_sel).
//
// ============================================================================

`include "mp64_pkg.vh"

module mp64_field_alu (
    input  wire        clk,
    input  wire        rst_n,

    // MMIO bus interface (same as mp64_x25519)
    input  wire        req,
    input  wire [5:0]  addr,       // byte offset within 64-byte block
    input  wire [63:0] wdata,
    input  wire        wen,
    output reg  [63:0] rdata,
    output reg         ack
);

    // ========================================================================
    // Constants
    // ========================================================================
    localparam [255:0] PRIME     = 256'h7FFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFED;
    localparam [255:0] A24       = 256'd121665;
    localparam [255:0] P_MINUS_2 = 256'h7FFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFEB;

    // secp256k1: p = 2^256 - 2^32 - 977
    localparam [255:0] PRIME_SECP     = 256'hFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFE_FFFFFC2F;
    localparam [255:0] P_MINUS_2_SECP = 256'hFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFE_FFFFFC2D;

    // NIST P-256: p = 2^256 - 2^224 + 2^192 + 2^96 - 1
    localparam [255:0] PRIME_P256     = 256'hFFFFFFFF_00000001_00000000_00000000_00000000_FFFFFFFF_FFFFFFFF_FFFFFFFF;
    localparam [255:0] P_MINUS_2_P256 = 256'hFFFFFFFF_00000001_00000000_00000000_00000000_FFFFFFFF_FFFFFFFF_FFFFFFFD;

    // Mode encoding
    localparam [3:0] MODE_X25519      = 4'd0,
                     MODE_FADD        = 4'd1,
                     MODE_FSUB        = 4'd2,
                     MODE_FMUL        = 4'd3,
                     MODE_FSQR        = 4'd4,
                     MODE_FINV        = 4'd5,
                     MODE_FPOW        = 4'd6,
                     MODE_MUL_RAW     = 4'd7,
                     MODE_FCMOV       = 4'd8,
                     MODE_FCEQ        = 4'd9,
                     MODE_LOAD_PRIME  = 4'd10,
                     MODE_FMAC        = 4'd11,
                     MODE_MUL_ADD_RAW = 4'd12;

    // ========================================================================
    // State machine
    // ========================================================================
    localparam [3:0] S_IDLE       = 4'd0,
                     S_CLAMP      = 4'd1,
                     S_LADDER     = 4'd2,
                     S_FINAL_SWAP = 4'd3,
                     S_POWER      = 4'd4,   // shared: X25519 invert / FINV / FPOW
                     S_FINAL_MUL  = 4'd5,   // X25519 final: X2 · Z2⁻¹
                     S_DONE       = 4'd6,
                     S_COMPUTE    = 4'd7,   // single-cycle modes 1-4, 7-12
                     S_REDC       = 4'd8;   // Montgomery reduction (reserved)

    reg [3:0]  state;
    reg        busy, done;
    reg [3:0]  mode_reg;
    reg        result_sel;            // 0 = LO, 1 = HI
    reg [1:0]  prime_sel;             // 0=25519, 1=secp256k1, 2=P-256, 3=custom

    // ========================================================================
    // I/O registers
    // ========================================================================
    reg [255:0] operand_a;            // scalar (mode 0) / operand A
    reg [255:0] operand_b;            // point  (mode 0) / operand B
    reg [255:0] result_lo;            // low 256 bits of result
    reg [255:0] result_hi;            // high 256 bits (MUL_RAW only)

    // ========================================================================
    // Montgomery ladder state (mode 0 only)
    // ========================================================================
    reg [255:0] X2, Z2, X3, Z3;
    reg         swap_bit;
    reg [7:0]   bit_idx;

    reg [255:0] rA, rB;
    reg [255:0] rAA, rBB;
    reg [255:0] rE;
    reg [255:0] rC, rD;
    reg [255:0] rDA, rCB;
    reg [255:0] rT;

    reg [3:0]   ladder_phase;

    // ========================================================================
    // Exponentiation state (modes 0, 5, 6)
    // ========================================================================
    reg [255:0] pow_acc;
    reg [255:0] pow_base;
    reg [255:0] pow_exp;              // P_MINUS_2 or operand_b
    reg [8:0]   pow_step;
    reg [1:0]   pow_phase;

    // ========================================================================
    // Field arithmetic (combinational)
    // ========================================================================

    function [255:0] field_add;
        input [255:0] a, b;
        reg [256:0] sum;
        begin
            sum = {1'b0, a} + {1'b0, b};
            if (sum >= {1'b0, PRIME})
                field_add = sum - {1'b0, PRIME};
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
                field_sub = PRIME - (b - a);
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
            if (r >= {1'b0, PRIME})  r = r - {1'b0, PRIME};
            if (r >= {1'b0, PRIME})  r = r - {1'b0, PRIME};
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

    // ========================================================================
    // secp256k1 reduction: 2^256 ≡ 2^32 + 977 (mod p)
    // ========================================================================

    function [255:0] field_reduce_secp;
        input [511:0] x;
        reg [289:0] hi_times_c;
        reg [289:0] fold1;
        reg [256:0] ov_times_c;
        reg [256:0] fold2;
        begin
            // First fold: lo + hi × (2^32 + 977)
            hi_times_c = x[511:256] * 290'd4294968273;
            fold1 = {34'd0, x[255:0]} + hi_times_c;
            // Second fold on overflow bits
            ov_times_c = fold1[289:256] * 257'd4294968273;
            fold2 = {1'b0, fold1[255:0]} + ov_times_c;
            if (fold2 >= {1'b0, PRIME_SECP})
                fold2 = fold2 - {1'b0, PRIME_SECP};
            field_reduce_secp = fold2[255:0];
        end
    endfunction

    // ========================================================================
    // NIST P-256 reduction: FIPS 186-4 §D.2 word-based
    // ========================================================================

    function [255:0] field_reduce_p256;
        input [511:0] x;
        reg [31:0] c [0:15];
        reg [255:0] s1, s2, s3, s4, s5, d1, d2, d3, d4;
        reg [263:0] pos, neg;
        reg [264:0] adj;
        integer i;
        begin
            // Extract sixteen 32-bit words
            for (i = 0; i < 16; i = i + 1)
                c[i] = x[i*32 +: 32];

            // FIPS 186-4 §D.2 terms (MSW..LSW)
            s1 = {c[7],  c[6],  c[5],  c[4],  c[3],  c[2],  c[1],  c[0]};
            s2 = {c[15], c[14], c[13], c[12], c[11], 32'd0, 32'd0, 32'd0};
            s3 = {32'd0, c[15], c[14], c[13], c[12], 32'd0, 32'd0, 32'd0};
            s4 = {c[15], c[14], 32'd0, 32'd0, 32'd0, c[10], c[9],  c[8]};
            s5 = {c[8],  c[13], c[15], c[14], c[13], c[11], c[10], c[9]};
            d1 = {c[10], c[8],  32'd0, 32'd0, 32'd0, c[13], c[12], c[11]};
            d2 = {c[11], c[9],  32'd0, 32'd0, c[15], c[14], c[13], c[12]};
            d3 = {c[12], 32'd0, c[10], c[9],  c[8],  c[15], c[14], c[13]};
            d4 = {c[13], 32'd0, c[11], c[10], c[9],  32'd0, c[15], c[14]};

            // Positive: s1 + 2*s2 + 2*s3 + s4 + s5
            pos = s1 + s2 + s2 + s3 + s3 + s4 + s5;
            // Negative: d1 + d2 + d3 + d4
            neg = d1 + d2 + d3 + d4;

            // Add 5*p to ensure non-negative result
            adj = {1'b0, pos}
                + {9'd0, PRIME_P256} + {9'd0, PRIME_P256}
                + {9'd0, PRIME_P256} + {9'd0, PRIME_P256}
                + {9'd0, PRIME_P256}
                - {1'b0, neg};

            // Reduce mod p (bounded: at most 12 subtracts)
            for (i = 0; i < 16; i = i + 1)
                if (adj >= {9'd0, PRIME_P256})
                    adj = adj - {9'd0, PRIME_P256};

            field_reduce_p256 = adj[255:0];
        end
    endfunction

    // ========================================================================
    // Prime-parameterized dispatch functions
    // ========================================================================

    function [255:0] field_add_sel;
        input [255:0] a, b;
        input [1:0] sel;
        reg [256:0] sum;
        reg [255:0] p;
        begin
            case (sel)
                2'd0:    p = PRIME;
                2'd1:    p = PRIME_SECP;
                2'd2:    p = PRIME_P256;
                default: p = PRIME;      // placeholder for custom
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
        reg [255:0] p;
        begin
            case (sel)
                2'd0:    p = PRIME;
                2'd1:    p = PRIME_SECP;
                2'd2:    p = PRIME_P256;
                default: p = PRIME;
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
        reg [511:0] prod;
        begin
            prod = a * b;
            case (sel)
                2'd0:    field_mul_sel = field_reduce(prod);
                2'd1:    field_mul_sel = field_reduce_secp(prod);
                2'd2:    field_mul_sel = field_reduce_p256(prod);
                default: field_mul_sel = field_reduce(prod);
            endcase
        end
    endfunction

    // Effective prime: X25519 (mode 0) always uses Curve25519 prime
    wire [1:0] eff_prime = (mode_reg == MODE_X25519) ? 2'd0 : prime_sel;

    // ========================================================================
    // Main sequential logic
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            operand_a    <= 256'd0;
            operand_b    <= 256'd0;
            result_lo    <= 256'd0;
            result_hi    <= 256'd0;
            state        <= S_IDLE;
            busy         <= 1'b0;
            done         <= 1'b0;
            mode_reg     <= 4'd0;
            result_sel   <= 1'b0;
            prime_sel    <= 2'd0;
            swap_bit     <= 1'b0;
            bit_idx      <= 8'd0;
            ladder_phase <= 4'd0;
            X2 <= 0; Z2 <= 0; X3 <= 0; Z3 <= 0;
            rA <= 0; rB <= 0; rAA <= 0; rBB <= 0; rE <= 0;
            rC <= 0; rD <= 0; rDA <= 0; rCB <= 0; rT <= 0;
            pow_acc  <= 0; pow_base <= 0; pow_exp <= 0;
            pow_step <= 0; pow_phase <= 0;
        end else begin

            // ================================================================
            // MMIO writes — operand load + command dispatch
            // ================================================================
            if (req && wen) begin
                case (addr[5:3])
                    3'b000: operand_a[ 63:  0] <= wdata;
                    3'b001: operand_a[127: 64] <= wdata;
                    3'b010: operand_a[191:128] <= wdata;
                    3'b011: operand_a[255:192] <= wdata;
                    3'b100: operand_b[ 63:  0] <= wdata;
                    3'b101: operand_b[127: 64] <= wdata;
                    3'b110: operand_b[191:128] <= wdata;
                    3'b111: begin
                        if (addr[2:0] == 3'b000)
                            operand_b[255:192] <= wdata;
                        else begin
                            // CMD byte
                            if (wdata[0] && (state == S_IDLE || state == S_DONE)) begin
                                // GO — dispatch by mode
                                mode_reg   <= wdata[4:1];
                                result_sel <= 1'b0;
                                busy       <= 1'b1;
                                done       <= 1'b0;

                                case (wdata[4:1])
                                    MODE_X25519: state <= S_CLAMP;
                                    MODE_FADD,
                                    MODE_FSUB,
                                    MODE_FMUL,
                                    MODE_FSQR,
                                    MODE_MUL_RAW,
                                    MODE_FCMOV,
                                    MODE_FCEQ,
                                    MODE_FMAC,
                                    MODE_MUL_ADD_RAW: state <= S_COMPUTE;
                                    MODE_FINV: begin
                                        pow_acc   <= 256'd1;
                                        pow_base  <= operand_a;
                                        case (prime_sel)
                                            2'd1:    pow_exp <= P_MINUS_2_SECP;
                                            2'd2:    pow_exp <= P_MINUS_2_P256;
                                            default: pow_exp <= P_MINUS_2;
                                        endcase
                                        pow_step  <= 9'd0;
                                        pow_phase <= 2'd0;
                                        state     <= S_POWER;
                                    end
                                    MODE_FPOW: begin
                                        pow_acc   <= 256'd1;
                                        pow_base  <= operand_a;
                                        pow_exp   <= operand_b;
                                        pow_step  <= 9'd0;
                                        pow_phase <= 2'd0;
                                        state     <= S_POWER;
                                    end
                                    default: state <= S_COMPUTE;
                                endcase
                            end else if (!wdata[0]) begin
                                // No go — update result_sel and prime_sel
                                result_sel <= wdata[5];
                                prime_sel  <= wdata[7:6];
                            end
                        end
                    end
                endcase
            end

            // ================================================================
            // State machine
            // ================================================================
            case (state)

                // ------------------------------------------------------------
                // S_COMPUTE — single-cycle operations (modes 1-4, 7)
                // ------------------------------------------------------------
                S_COMPUTE: begin
                    result_hi <= 256'd0;
                    case (mode_reg)
                        MODE_FADD: result_lo <= field_add_sel(operand_a, operand_b, prime_sel);
                        MODE_FSUB: result_lo <= field_sub_sel(operand_a, operand_b, prime_sel);
                        MODE_FMUL: result_lo <= field_mul_sel(operand_a, operand_b, prime_sel);
                        MODE_FSQR: result_lo <= field_mul_sel(operand_a, operand_a, prime_sel);
                        MODE_MUL_RAW: begin
                            result_lo <= (operand_a * operand_b);
                            result_hi <= (operand_a * operand_b) >> 256;
                        end
                        default: result_lo <= 256'd0;
                    endcase
                    busy  <= 1'b0;
                    done  <= 1'b1;
                    state <= S_DONE;
                end

                // ------------------------------------------------------------
                // S_CLAMP — mode 0: apply RFC 7748 clamping, init ladder
                // ------------------------------------------------------------
                S_CLAMP: begin
                    operand_a      <= clamp(operand_a);
                    operand_b[255] <= 1'b0;
                    X2 <= 256'd1;
                    Z2 <= 256'd0;
                    X3 <= operand_b;
                    X3[255] <= 1'b0;
                    Z3 <= 256'd1;
                    bit_idx      <= 8'd254;
                    swap_bit     <= 1'b0;
                    ladder_phase <= 4'd0;
                    state        <= S_LADDER;
                end

                // ------------------------------------------------------------
                // S_LADDER — Montgomery ladder (RFC 7748 §5)
                // 14 phases per scalar bit (see mp64_x25519.v for details)
                // ------------------------------------------------------------
                S_LADDER: begin
                    case (ladder_phase)
                        4'd0: begin
                            if (swap_bit ^ operand_a[bit_idx]) begin
                                X2 <= X3;  Z2 <= Z3;
                                X3 <= X2;  Z3 <= Z2;
                                rA <= field_add(X3, Z3);
                                rB <= field_sub(X3, Z3);
                            end else begin
                                rA <= field_add(X2, Z2);
                                rB <= field_sub(X2, Z2);
                            end
                            swap_bit <= operand_a[bit_idx];
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
                        4'd11: begin Z3  <= field_mul(operand_b, rCB); ladder_phase <= 4'd12; end
                        4'd12: begin Z2  <= field_mul(rE, rT);     ladder_phase <= 4'd13; end
                        4'd13: begin
                            if (bit_idx == 8'd0)
                                state <= S_FINAL_SWAP;
                            else begin
                                bit_idx <= bit_idx - 8'd1;
                                ladder_phase <= 4'd0;
                            end
                        end
                        default: ladder_phase <= 4'd0;
                    endcase
                end

                // ------------------------------------------------------------
                // S_FINAL_SWAP — cswap after ladder (mode 0)
                // ------------------------------------------------------------
                S_FINAL_SWAP: begin
                    if (swap_bit) begin
                        X2 <= X3;  Z2 <= Z3;
                        X3 <= X2;  Z3 <= Z2;
                        pow_base <= Z3;
                    end else begin
                        pow_base <= Z2;
                    end
                    pow_acc   <= 256'd1;
                    pow_exp   <= P_MINUS_2;
                    pow_step  <= 9'd0;
                    pow_phase <= 2'd0;
                    state     <= S_POWER;
                end

                // ------------------------------------------------------------
                // S_POWER — binary square-and-multiply exponentiation
                //
                // Shared by mode 0 (Z2 inversion), mode 5 (FINV), mode 6 (FPOW).
                // Iterates MSB-first through pow_exp (bit 254 downto 0).
                // 255 steps × (square + cond-mul + advance) ≈ 765 cycles.
                // ------------------------------------------------------------
                S_POWER: begin
                    case (pow_phase)
                        2'd0: begin
                            pow_acc <= field_mul_sel(pow_acc, pow_acc, eff_prime);
                            pow_phase <= 2'd1;
                        end
                        2'd1: begin
                            if (pow_exp[255 - pow_step[7:0]])
                                pow_acc <= field_mul_sel(pow_acc, pow_base, eff_prime);
                            pow_phase <= 2'd2;
                        end
                        2'd2: begin
                            if (pow_step == 9'd255) begin
                                // Exponentiation complete
                                if (mode_reg == MODE_X25519)
                                    state <= S_FINAL_MUL;
                                else begin
                                    // FINV or FPOW — result is pow_acc
                                    result_lo <= pow_acc;
                                    result_hi <= 256'd0;
                                    busy  <= 1'b0;
                                    done  <= 1'b1;
                                    state <= S_DONE;
                                end
                            end else begin
                                pow_step  <= pow_step + 9'd1;
                                pow_phase <= 2'd0;
                            end
                        end
                        default: pow_phase <= 2'd0;
                    endcase
                end

                // ------------------------------------------------------------
                // S_FINAL_MUL — mode 0: result = X2 · Z2⁻¹
                // ------------------------------------------------------------
                S_FINAL_MUL: begin
                    result_lo <= field_mul(X2, pow_acc);
                    result_hi <= 256'd0;
                    busy  <= 1'b0;
                    done  <= 1'b1;
                    state <= S_DONE;
                end

                // S_DONE — idle until next CMD
                S_DONE: begin
                    // CMD dispatch handled in the write section above
                end

            endcase
        end
    end

    // ========================================================================
    // MMIO read handler
    // ========================================================================
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            rdata <= 64'd0;
            ack   <= 1'b0;
        end else begin
            ack <= 1'b0;
            if (req) begin
                ack <= 1'b1;
                if (!wen) begin
                    case (addr[5:3])
                        3'b000: rdata <= {62'd0, done, busy};
                        3'b001: rdata <= result_sel ? result_hi[ 63:  0] : result_lo[ 63:  0];
                        3'b010: rdata <= result_sel ? result_hi[127: 64] : result_lo[127: 64];
                        3'b011: rdata <= result_sel ? result_hi[191:128] : result_lo[191:128];
                        3'b100: rdata <= result_sel ? result_hi[255:192] : result_lo[255:192];
                        default: rdata <= 64'd0;
                    endcase
                end else begin
                    rdata <= 64'd0;
                end
            end
        end
    end

endmodule
